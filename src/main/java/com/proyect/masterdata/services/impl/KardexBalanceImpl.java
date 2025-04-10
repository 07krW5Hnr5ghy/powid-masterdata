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

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Log4j2
public class KardexBalanceImpl implements IKardexBalance {
    private final KardexBalanceRepository kardexBalanceRepository;
    @Override
    public KardexBalance save(RequestKardexBalance requestKardexBalance) throws BadRequestExceptions, InternalErrorExceptions {
        KardexBalance kardexBalance;
        KardexBalance kardexBalanceResult = null;
        try{
            kardexBalance = kardexBalanceRepository.findOldestByClientIdAndProductIdWithStock(
                    requestKardexBalance.getUser().getClientId(),
                    requestKardexBalance.getProduct().getId()
            );
            
            if(kardexBalance==null&&requestKardexBalance.getAdd()){
                Long lotNumber = kardexBalanceRepository.countByClientIdAndProductId(requestKardexBalance.getUser().getClientId(),requestKardexBalance.getProduct().getId())+1L;
                kardexBalanceResult = kardexBalanceRepository.save(KardexBalance.builder()
                                .quantity(requestKardexBalance.getQuantity())
                                .lotNumber(lotNumber)
                                .user(requestKardexBalance.getUser())
                                .userId(requestKardexBalance.getUser().getId())
                                .client(requestKardexBalance.getUser().getClient())
                                .clientId(requestKardexBalance.getUser().getClientId())
                                .unitPrice(requestKardexBalance.getUnitPrice())
                        .build());
            }

            if(kardexBalance!=null){
                if(requestKardexBalance.getAdd()){
                    kardexBalance.setQuantity(kardexBalance.getQuantity()+requestKardexBalance.getQuantity());
                }else{
                    kardexBalance.setQuantity(kardexBalance.getQuantity()-requestKardexBalance.getQuantity());
                }
                kardexBalanceResult = kardexBalanceRepository.save(kardexBalance);
            }
            return kardexBalanceResult;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
