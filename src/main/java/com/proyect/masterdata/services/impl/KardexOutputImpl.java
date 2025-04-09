package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.KardexInput;
import com.proyect.masterdata.domain.KardexOperationType;
import com.proyect.masterdata.domain.KardexOutput;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestKardexBalance;
import com.proyect.masterdata.dto.request.RequestKardexOutput;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.KardexBalanceRepository;
import com.proyect.masterdata.repository.KardexOperationTypeRepository;
import com.proyect.masterdata.repository.KardexOutputRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IKardexBalance;
import com.proyect.masterdata.services.IkardexOutput;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;

@Service
@RequiredArgsConstructor
@Log4j2
public class KardexOutputImpl implements IkardexOutput {
    private final UserRepository userRepository;
    private final KardexOutputRepository kardexOutputRepository;
    private final KardexOperationTypeRepository kardexOperationTypeRepository;
    private final KardexBalanceRepository kardexBalanceRepository;
    private IKardexBalance iKardexBalance;
    @Override
    public KardexOutput save(RequestKardexOutput requestKardexOutput) throws BadRequestExceptions, InternalErrorExceptions {
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
            KardexOutput kardexOutput =  kardexOutputRepository.save(KardexOutput.builder()
                    .quantity(requestKardexOutput.getQuantity())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .user(user)
                    .userId(user.getId())
                    .registrationDate(OffsetDateTime.now())
                    .kardexOperationType(kardexOperationType)
                    .kardexOperationTypeId(kardexOperationType.getId())
                    .build());
            RequestKardexBalance requestKardexBalance = RequestKardexBalance.builder()
                    .product(requestKardexOutput.getProduct())
                    .quantity(requestKardexOutput.getQuantity())
                    .user(user)
                    .add(false)
                    .build();
            iKardexBalance.save(requestKardexBalance);
            return kardexOutput;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
