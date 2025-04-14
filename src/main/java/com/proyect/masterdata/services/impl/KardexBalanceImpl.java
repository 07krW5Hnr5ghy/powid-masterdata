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
            List<KardexBalance> kardexBalanceList = kardexBalanceRepository.findAllByClientIdAndProductIdAndWarehouseIdWithStock(
                    requestKardexBalance.getUser().getClientId(),
                    requestKardexBalance.getProduct().getId(),
                    requestKardexBalance.getWarehouse().getId()
            );
            
            if(kardexBalance==null&&requestKardexBalance.getAdd()){
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
            }

            if(kardexBalance!=null&&requestKardexBalance.getAdd()){
                kardexBalance.setRemainingQuantity(kardexBalance.getRemainingQuantity()+requestKardexBalance.getQuantity());
            }

            if(kardexBalance!=null){
                if(requestKardexBalance.getAdd()){
                    kardexBalance.setRemainingQuantity(kardexBalance.getRemainingQuantity()+requestKardexBalance.getQuantity());
                }else{
                    Integer leftQuantity = requestKardexBalance.getQuantity();

                    if(kardexBalance.getRemainingQuantity()>requestKardexBalance.getQuantity()){
                        kardexBalance.setRemainingQuantity(kardexBalance.getRemainingQuantity()-requestKardexBalance.getQuantity());
                    }
                }
                kardexBalance.setUpdateDate(OffsetDateTime.now());
                kardexBalanceResult = kardexBalanceRepository.save(kardexBalance);
            }
            return kardexBalanceResult;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
