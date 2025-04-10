package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.KardexInput;
import com.proyect.masterdata.domain.KardexOperationType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestKardexBalance;
import com.proyect.masterdata.dto.request.RequestKardexInput;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.KardexInputRepository;
import com.proyect.masterdata.repository.KardexOperationTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IKardexBalance;
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
    private final IKardexBalance iKardexBalance;
    @Override
    public KardexInput save(RequestKardexInput requestKardexInput) throws BadRequestExceptions, InternalErrorExceptions {
        KardexOperationType kardexOperationType;
        try{
            kardexOperationType = kardexOperationTypeRepository.findByNameAndClientId("COMPRA",requestKardexInput.getUser().getClientId());

        }catch (RuntimeException e){
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
                    .add(true)
                    .build();
            iKardexBalance.save(requestKardexBalance);
            return kardexInput;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
