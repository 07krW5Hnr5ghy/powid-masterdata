package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentState;
import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.response.ResponsePaymentState;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.PaymentStateMapper;
import com.proyect.masterdata.repository.PaymentStateRepository;
import com.proyect.masterdata.services.IPaymentState;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
@Service
@RequiredArgsConstructor
public class PaymentStateImpl implements IPaymentState {

    private final PaymentStateRepository paymentStateRepository;
    private final PaymentStateMapper paymentStateMapper;

    @Override
    public List<PaymentStateDTO> listPaymentState() throws BadRequestExceptions {
        return paymentStateMapper.paymentStateListToPaymentStateListDTO(paymentStateRepository.findAll());
    }

    @Override
    public ResponsePaymentState addPaymentState(String paymentState) throws BadRequestExceptions {
        try{
            paymentStateRepository.save(PaymentState.builder().name(paymentState).status(true).build());
            return ResponsePaymentState.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponsePaymentState deletePaymentState(Long id) throws BadRequestExceptions {
        try{
            PaymentState record = paymentStateRepository.findById(id).get();
            paymentStateRepository.save(PaymentState.builder().name(record.getName()).dateRegistration(new Date(System.currentTimeMillis())).id(record.getId()).status(false).build());
            return ResponsePaymentState.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public PaymentStateDTO updatePaymentState(String name,Long id) throws BadRequestExceptions {
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
