package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.KardexInput;
import com.proyect.masterdata.domain.KardexOperationType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestKardexInput;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.KardexInputRepository;
import com.proyect.masterdata.repository.KardexOperationTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IKardexInput;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;

@Service
@RequiredArgsConstructor
@Log4j2
public class KardexInputImpl implements IKardexInput {
    private final UserRepository userRepository;
    private final KardexInputRepository kardexInputRepository;
    private final KardexOperationTypeRepository kardexOperationTypeRepository;
    @Override
    public KardexInput save(RequestKardexInput requestKardexInput) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        KardexOperationType kardexOperationType;
        try{
            user = userRepository.findByUsernameAndStatusTrue(requestKardexInput.getUser().toUpperCase());

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
            Long lotNumber = kardexInputRepository.countByClientIdAndProductId(user.getClientId(),requestKardexInput.getProduct().getId());
            return kardexInputRepository.save(KardexInput.builder()
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .lotNumber(lotNumber)
                            .product(requestKardexInput.getProduct())
                            .productId(requestKardexInput.getProduct().getId())
                            .user(user)
                            .userId(user.getId())
                            .registrationDate(OffsetDateTime.now())
                            .supplyOrderItem(requestKardexInput.getSupplyOrderItem())
                            .supplyOrderItemId(requestKardexInput.getSupplyOrderItem().getId())
                            .kardexOperationType(kardexOperationType)
                            .kardexOperationTypeId(kardexOperationType.getId())
                    .build());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
