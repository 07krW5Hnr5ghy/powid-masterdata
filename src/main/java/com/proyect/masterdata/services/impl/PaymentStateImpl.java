package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.domain.PaymentState;
import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.PaymentStateMapper;
import com.proyect.masterdata.repository.PaymentStateRepository;
import com.proyect.masterdata.services.IPaymentState;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
@Service
@RequiredArgsConstructor
public class PaymentStateImpl implements IPaymentState {

    private final PaymentStateRepository paymentStateRepository;
    private final PaymentStateMapper paymentStateMapper;

    @Override
    public List<PaymentStateDTO> listPaymentState() throws BadRequestExceptions {
        paymentStateRepository.save(PaymentState.builder().name("Por Recaudar").build());
        paymentStateRepository.save(PaymentState.builder().name("Recaudado").build());
        return paymentStateMapper.paymentStateListToPaymentStateListDTO(paymentStateRepository.findAll());
    }

    @Override
    public void addPaymentState(String paymentState) throws BadRequestExceptions {
        paymentStateRepository.save(PaymentState.builder().name(paymentState).build());
        System.out.println("Payment method : " + paymentState + " was added to the table");
    }

}
