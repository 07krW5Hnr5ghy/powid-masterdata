package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentState;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.PaymentStateMapper;
import com.proyect.masterdata.repository.PaymentStateRepository;
import com.proyect.masterdata.services.IMasterList;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
@Service
@RequiredArgsConstructor
public class PaymentStateImpl implements IMasterList {

    private final PaymentStateRepository paymentStateRepository;
    private final PaymentStateMapper paymentStateMapper;

    @Override
    public List<MasterListDTO> listRecords() throws BadRequestExceptions {
        return paymentStateMapper.paymentStateListToPaymentStateListDTO(paymentStateRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name) throws BadRequestExceptions {
        try{
            paymentStateRepository.save(PaymentState.builder().name(name).status(true).build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions {
        try{
            PaymentState paymentState = paymentStateRepository.findById(id).get();
            paymentStateRepository.save(PaymentState.builder()
                    .name(paymentState.getName())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .id(paymentState.getId())
                    .status(false)
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
    public MasterListDTO updateRecord(String name,Long id) throws BadRequestExceptions {
        try{
            PaymentState paymentState = paymentStateRepository.save(PaymentState.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .status(true)
                    .build());
            return PaymentStateMapper.INSTANCE.paymentStateToPaymentStateDTO(paymentState);
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

}
