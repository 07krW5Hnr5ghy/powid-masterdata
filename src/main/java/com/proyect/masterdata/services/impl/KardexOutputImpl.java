package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.KardexInputDTO;
import com.proyect.masterdata.dto.KardexOutputDTO;
import com.proyect.masterdata.dto.projections.KardexOutputProjection;
import com.proyect.masterdata.dto.request.RequestKardexBalance;
import com.proyect.masterdata.dto.request.RequestKardexOutput;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IKardexBalance;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.services.IkardexOutput;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class KardexOutputImpl implements IkardexOutput {
    private final UserRepository userRepository;
    private final KardexOutputRepository kardexOutputRepository;
    private final KardexOperationTypeRepository kardexOperationTypeRepository;
    private final KardexBalanceRepository kardexBalanceRepository;
    private final KardexOutputRepositoryCustom kardexOutputRepositoryCustom;
    private final IUtil iUtil;
    private final IKardexBalance iKardexBalance;
    @Override
    public void save(RequestKardexOutput requestKardexOutput) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        KardexOperationType kardexOperationType;
        try{
            user = userRepository.findByUsernameAndStatusTrue(requestKardexOutput.getUser().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(user==null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            kardexOperationType = kardexOperationTypeRepository.findByNameAndClientId("COMPRA",user.getClientId());
        }
        try {
            List<KardexBalance> kardexBalanceList = kardexBalanceRepository.findAllByClientIdAndProductIdAndWarehouseIdWithStock(
                    user.getClientId(),
                    requestKardexOutput.getProduct().getId(),
                    requestKardexOutput.getWarehouse().getId()
            );
            int remainingToDeduct = requestKardexOutput.getQuantity();
            for (KardexBalance kardexBalance : kardexBalanceList){
                KardexOutput kardexOutput =  KardexOutput.builder()
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .user(user)
                        .userId(user.getId())
                        .orderNumber(requestKardexOutput.getOrderNumber())
                        .lotNumber(kardexBalance.getLotNumber())
                        .unitPrice(kardexBalance.getUnitPrice())
                        .registrationDate(OffsetDateTime.now())
                        .product(requestKardexOutput.getProduct())
                        .productId(requestKardexOutput.getProduct().getId())
                        .kardexOperationType(kardexOperationType)
                        .kardexOperationTypeId(kardexOperationType.getId())
                        .deliveryManifestItemId(requestKardexOutput.getDeliveryManifestItemId())
                        .warehouse(requestKardexOutput.getWarehouse())
                        .warehouseId(requestKardexOutput.getWarehouse().getId())
                        .build();
                if (remainingToDeduct <= 0) break;
                int available = kardexBalance.getRemainingQuantity();
                if (available >= remainingToDeduct) {
                    kardexBalance.setRemainingQuantity(available - remainingToDeduct);
                    kardexOutput.setQuantity(available-remainingToDeduct);
                    remainingToDeduct = 0;
                } else {
                    kardexBalance.setRemainingQuantity(0);
                    kardexOutput.setQuantity(available);
                    remainingToDeduct -= available;
                }
                kardexOutputRepository.save(kardexOutput);
                kardexBalanceRepository.save(kardexBalance);
            }
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<Page<KardexOutputDTO>> list(
            String user,
            Integer quantity,
            String product,
            UUID productId,
            String username,
            String warehouse,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<KardexOutput> kardexOutputPage;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                kardexOutputPage = kardexOutputRepositoryCustom.searchForKardexOutput(
                        clientId,
                        quantity,
                        product,
                        productId,
                        username,
                        warehouse,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                e.printStackTrace();
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (kardexOutputPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<KardexOutputDTO> kardexOutputDTOs = kardexOutputPage.getContent().stream()
                    .map(kardexOutput -> KardexOutputDTO.builder()
                            .id(kardexOutput.getId())
                            .user(kardexOutput.getUser().getUsername())
                            .quantity(kardexOutput.getQuantity())
                            .product(kardexOutput.getProduct().getName())
                            .productSku(iUtil.buildProductSku(kardexOutput.getProduct()))
                            .categoryProduct(kardexOutput.getProduct().getSubCategoryProduct().getCategoryProduct().getName())
                            .subCategoryProduct(kardexOutput.getProduct().getSubCategoryProduct().getName())
                            .model(kardexOutput.getProduct().getModel().getName())
                            .color(kardexOutput.getProduct().getColor().getName())
                            .size(kardexOutput.getProduct().getSize().getName())
                            .registrationDate(kardexOutput.getRegistrationDate())
                            .value(kardexOutput.getUnitPrice()*kardexOutput.getQuantity())
                            .warehouse(kardexOutput.getWarehouse().getName())
                            .orderNumber(kardexOutput.getOrderNumber())
                            .lotNumber(kardexOutput.getLotNumber())
                            .build())
                    .toList();

            return new PageImpl<>(kardexOutputDTOs, kardexOutputPage.getPageable(),
                    kardexOutputPage.getTotalElements());
        });
    }

    @Override
    public List<KardexOutputDTO> test(String username,UUID deliveryManifestItemId) {
        List<Object[]> kardexOutputList;
        User user;
        try {
            user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            System.out.println(deliveryManifestItemId);
            System.out.println(user.getClientId());
            kardexOutputList = kardexOutputRepository.selectAllByDeliveryManifestItemIdAndClientId(
                    user.getClientId(),
                    deliveryManifestItemId
            );
            System.out.println(kardexOutputList);
            System.out.println(kardexOutputList.size());
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        try {
            System.out.println(kardexOutputList);
            List<KardexOutputDTO> kardexOutputDTOS = new ArrayList<>();
            for(Object[] kardexOutput:kardexOutputList){
                System.out.println(kardexOutput[0]);
                System.out.println(kardexOutput[1]);
                System.out.println(kardexOutput[2]);
                kardexOutputDTOS.add(KardexOutputDTO.builder()
                                .lotNumber((Long) kardexOutput[2])
                        .build());
            }
            return kardexOutputDTOS;
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
