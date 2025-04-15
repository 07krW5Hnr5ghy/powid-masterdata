package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.KardexBalance;
import com.proyect.masterdata.dto.request.RequestKardexBalance;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.KardexBalanceRepository;
import com.proyect.masterdata.services.IKardexBalance;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Log4j2
public class KardexBalanceImpl implements IKardexBalance {
    private final KardexBalanceRepository kardexBalanceRepository;
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
}
