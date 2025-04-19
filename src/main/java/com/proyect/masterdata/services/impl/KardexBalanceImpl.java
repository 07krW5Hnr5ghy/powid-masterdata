package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.KardexBalance;
import com.proyect.masterdata.dto.KardexBalanceDTO;
import com.proyect.masterdata.dto.request.RequestKardexBalance;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.KardexBalanceRepository;
import com.proyect.masterdata.repository.KardexBalanceRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IKardexBalance;
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
public class KardexBalanceImpl implements IKardexBalance {
    private final KardexBalanceRepository kardexBalanceRepository;
    private final KardexBalanceRepositoryCustom kardexBalanceRepositoryCustom;
    private final UserRepository userRepository;
    private final IUtil iUtil;
    @Override
    public KardexBalance save(RequestKardexBalance requestKardexBalance) throws BadRequestExceptions, InternalErrorExceptions {
        KardexBalance kardexBalanceResult = null;
        try{

            if(requestKardexBalance.getAdd()){
                kardexBalanceResult = kardexBalanceRepository.save(KardexBalance.builder()
                                .remainingQuantity(requestKardexBalance.getQuantity())
                                .lotNumber(requestKardexBalance.getLotNumber())
                                .user(requestKardexBalance.getUser())
                                .userId(requestKardexBalance.getUser().getId())
                                .client(requestKardexBalance.getUser().getClient())
                                .clientId(requestKardexBalance.getUser().getClientId())
                                .unitPrice(requestKardexBalance.getUnitPrice())
                                .warehouse(requestKardexBalance.getWarehouse())
                                .warehouseId(requestKardexBalance.getWarehouse().getId())
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                        .build());
            }else{
                int remainingToDeduct = requestKardexBalance.getQuantity();
                List<KardexBalance> kardexBalanceList = kardexBalanceRepository.findAllByClientIdAndProductIdAndWarehouseIdWithStock(
                        requestKardexBalance.getUser().getClientId(),
                        requestKardexBalance.getProduct().getId(),
                        requestKardexBalance.getWarehouse().getId()
                );
                for (KardexBalance kardexBalance : kardexBalanceList) {
                    if (remainingToDeduct <= 0) break;

                    int available = kardexBalance.getRemainingQuantity();

                    if (available >= remainingToDeduct) {
                        kardexBalance.setRemainingQuantity(available - remainingToDeduct);
                        remainingToDeduct = 0;
                    } else {
                        kardexBalance.setRemainingQuantity(0);
                        remainingToDeduct -= available;
                    }

                    kardexBalanceRepository.save(kardexBalance);
                }
            }
            return kardexBalanceResult;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<Page<KardexBalanceDTO>> list(String user, Integer quantity, Long lotNumber, String product, UUID productId, String username, String warehouse, Double unitPrice, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<KardexBalance> kardexBalancePage;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                kardexBalancePage = kardexBalanceRepositoryCustom.searchForKardexBalance(
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

            if (kardexBalancePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<KardexBalanceDTO> kardexBalanceDTOs = kardexBalancePage.getContent().stream()
                    .map(kardexBalance -> KardexBalanceDTO.builder()
                            .id(kardexBalance.getId())
                            .user(kardexBalance.getUser().getUsername())
                            .quantity(kardexBalance.getRemainingQuantity())
                            .product(kardexBalance.getProduct().getName())
                            .productSku(iUtil.buildProductSku(kardexBalance.getProduct()))
                            .categoryProduct(kardexBalance.getProduct().getSubCategoryProduct().getCategoryProduct().getName())
                            .subCategoryProduct(kardexBalance.getProduct().getSubCategoryProduct().getName())
                            .model(kardexBalance.getProduct().getModel().getName())
                            .color(kardexBalance.getProduct().getColor().getName())
                            .size(kardexBalance.getProduct().getSize().getName())
                            .registrationDate(kardexBalance.getRegistrationDate())
                            .lotNumber(kardexBalance.getLotNumber())
                            .build())
                    .toList();

            return new PageImpl<>(kardexBalanceDTOs, kardexBalancePage.getPageable(),
                    kardexBalancePage.getTotalElements());
        });
    }
}
