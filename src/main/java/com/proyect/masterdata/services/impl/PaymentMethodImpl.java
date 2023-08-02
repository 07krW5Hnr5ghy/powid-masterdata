package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.PaymentMethodMapper;
import com.proyect.masterdata.repository.PaymentMethodRepository;
import com.proyect.masterdata.services.IMasterList;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class PaymentMethodImpl implements IMasterList {

    private final PaymentMethodRepository paymentMethodRepository;
    private final PaymentMethodMapper paymentMethodMapper;
    @Override
    public List<MasterListDTO> listRecords() throws BadRequestExceptions {
        return paymentMethodMapper.paymentMethodListToPaymentMethodListDTO(paymentMethodRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name) throws BadRequestExceptions {
        try{
            paymentMethodRepository.save(PaymentMethod.builder()
                    .name(name)
                    .status(true)
                    .build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions {
        try{
            PaymentMethod paymentMethod = paymentMethodRepository.findById(id).get();
            paymentMethodRepository.save(PaymentMethod.builder()
                    .name(paymentMethod.getName())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .id(paymentMethod.getId())
                    .status(false)
                    .build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public MasterListDTO updateRecord(String name,Long id) throws BadRequestExceptions {
        try{
            PaymentMethod paymentMethod = paymentMethodRepository.save(PaymentMethod.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .status(true)
                    .build());
            return PaymentMethodMapper.INSTANCE.paymentMethodToPaymentMethodDTO(paymentMethod);
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }
}
