package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.PaymentMethodMapper;
import com.proyect.masterdata.repository.PaymentMethodRepository;
import com.proyect.masterdata.services.IPaymentMethod;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import java.util.List;

@Service
@RequiredArgsConstructor
public class PaymentMethodImpl implements IPaymentMethod {

    private final PaymentMethodRepository paymentMethodRepository;
    private final PaymentMethodMapper paymentMethodMapper;
    @Override
    public List<PaymentMethodDTO> listPaymentMethod() throws BadRequestExceptions {
        paymentMethodRepository.save(PaymentMethod.builder().name("PLIN").build());
        paymentMethodRepository.save(PaymentMethod.builder().name("EFECTIVO").build());
        return paymentMethodMapper.paymentMethodListToPaymentMethodListDTO(paymentMethodRepository.findAll());
    }
}
