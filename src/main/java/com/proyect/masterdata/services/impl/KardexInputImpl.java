package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.KardexInputDTO;
import com.proyect.masterdata.dto.projections.KardexOutputProjection;
import com.proyect.masterdata.dto.request.RequestKardexBalance;
import com.proyect.masterdata.dto.request.RequestKardexInput;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IKardexBalance;
import com.proyect.masterdata.services.IKardexInput;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class KardexInputImpl implements IKardexInput {
    private final UserRepository userRepository;
    private final KardexInputRepository kardexInputRepository;
    private final KardexOperationTypeRepository kardexOperationTypeRepository;
    private final IKardexBalance iKardexBalance;
    private final KardexInputRepositoryCustom kardexInputRepositoryCustom;
    private final IUtil iUtil;
    private final KardexOutputRepository kardexOutputRepository;
    private final KardexBalanceRepository kardexBalanceRepository;
    @Override
    public KardexInput save(RequestKardexInput requestKardexInput) throws BadRequestExceptions, InternalErrorExceptions {
        KardexOperationType kardexOperationType;
        try{
            kardexOperationType = kardexOperationTypeRepository.findByNameAndClientId("COMPRA",requestKardexInput.getUser().getClientId());
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        try {
            Long lotNumber = kardexInputRepository.countByClientIdAndProductIdAndWarehouseId(requestKardexInput.getUser().getClientId(),requestKardexInput.getProduct().getId(),requestKardexInput.getWarehouse().getId())+1L;
            KardexInput kardexInput = kardexInputRepository.save(KardexInput.builder()
                            .client(requestKardexInput.getUser().getClient())
                            .clientId(requestKardexInput.getUser().getClientId())
                            .lotNumber(lotNumber)
                            .product(requestKardexInput.getProduct())
                            .productId(requestKardexInput.getProduct().getId())
                            .user(requestKardexInput.getUser())
                            .userId(requestKardexInput.getUser().getId())
                            .registrationDate(OffsetDateTime.now())
                            .kardexOperationType(kardexOperationType)
                            .kardexOperationTypeId(kardexOperationType.getId())
                            .warehouse(requestKardexInput.getWarehouse())
                            .warehouseId(requestKardexInput.getWarehouse().getId())
                            .quantity(requestKardexInput.getQuantity())
                            .unitPrice(requestKardexInput.getUnitPrice())
                    .build());
            RequestKardexBalance requestKardexBalance = RequestKardexBalance.builder()
                    .product(requestKardexInput.getProduct())
                    .quantity(requestKardexInput.getQuantity())
                    .user(requestKardexInput.getUser())
                    .unitPrice(requestKardexInput.getUnitPrice())
                    .lotNumber(kardexInput.getLotNumber())
                    .warehouse(kardexInput.getWarehouse())
                    .add(true)
                    .build();
            iKardexBalance.save(requestKardexBalance);
            return kardexInput;
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public void returnFromDeliveryManifestItem(UUID deliveryManifestItemId,Integer units, User user) throws BadRequestExceptions, InternalErrorExceptions {
        List<Object[]> kardexOutputList;
        try {
            System.out.println(deliveryManifestItemId);
            System.out.println(user.getClientId());
            kardexOutputList = kardexOutputRepository.selectAllByDeliveryManifestItemIdAndClientId(
                    user.getClientId(),
                    deliveryManifestItemId
            );
            System.out.println(kardexOutputList);
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        try {
            System.out.println(kardexOutputList);
            for(Object[] kardexOutput:kardexOutputList){
                System.out.println(kardexOutput[0]);
                System.out.println(kardexOutput[1]);
                System.out.println(kardexOutput[2]);
                KardexBalance kardexBalance = kardexBalanceRepository.findAllByClientIdAndProductIdAndWarehouseIdWithoutSockCheck(
                        user.getClientId(),
                        (UUID) kardexOutput[0],
                        (UUID) kardexOutput[1],
                        (Long) kardexOutput[2]
                );
                System.out.println(kardexBalance);
                if(kardexBalance!=null){
                    kardexBalance.setRemainingQuantity(kardexBalance.getRemainingQuantity()+units);
                    kardexBalance.setUpdateDate(OffsetDateTime.now());
                    kardexBalanceRepository.save(kardexBalance);
                }
            }
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<Page<KardexInputDTO>> list(
            String user,
            Integer quantity,
            Long lotNumber,
            String product,
            UUID productId,
            String username,
            String warehouse,
            Double unitPrice,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<KardexInput> kardexInputPage;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                kardexInputPage = kardexInputRepositoryCustom.searchForKardexInput(
                        clientId,
                        quantity,
                        lotNumber,
                        product,
                        productId,
                        username,
                        warehouse,
                        unitPrice,
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

            if (kardexInputPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<KardexInputDTO> kardexInputDTOs = kardexInputPage.getContent().stream()
                    .map(kardexInput -> KardexInputDTO.builder()
                            .id(kardexInput.getId())
                            .user(kardexInput.getUser().getUsername())
                            .quantity(kardexInput.getQuantity())
                            .product(kardexInput.getProduct().getName())
                            .productSku(iUtil.buildProductSku(kardexInput.getProduct()))
                            .categoryProduct(kardexInput.getProduct().getSubCategoryProduct().getCategoryProduct().getName())
                            .subCategoryProduct(kardexInput.getProduct().getSubCategoryProduct().getName())
                            .model(kardexInput.getProduct().getModel().getName())
                            .color(kardexInput.getProduct().getColor().getName())
                            .size(kardexInput.getProduct().getSize().getName())
                            .registrationDate(kardexInput.getRegistrationDate())
                            .lotNumber(kardexInput.getLotNumber())
                            .value(kardexInput.getUnitPrice()*kardexInput.getQuantity())
                            .warehouse(kardexInput.getWarehouse().getName())
                            .build())
                    .toList();

            return new PageImpl<>(kardexInputDTOs, kardexInputPage.getPageable(),
                    kardexInputPage.getTotalElements());
        });
    }
}
