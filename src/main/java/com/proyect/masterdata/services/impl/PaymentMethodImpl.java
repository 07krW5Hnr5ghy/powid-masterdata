package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.response.ResponsePaymentMethod;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.PaymentMethodMapper;
import com.proyect.masterdata.repository.PaymentMethodRepository;
import com.proyect.masterdata.services.IPaymentMethod;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class PaymentMethodImpl implements IPaymentMethod {

    private final PaymentMethodRepository paymentMethodRepository;
    private final PaymentMethodMapper paymentMethodMapper;
    @Override
    public List<PaymentMethodDTO> listPaymentMethod() throws BadRequestExceptions {
        return paymentMethodMapper.paymentMethodListToPaymentMethodListDTO(paymentMethodRepository.findAll());
    }

    @Override
    public ResponsePaymentMethod addPaymentMethod(String paymentMethod) throws BadRequestExceptions {
        try{
            paymentMethodRepository.save(PaymentMethod.builder().name(paymentMethod).status(true).build());
            return ResponsePaymentMethod.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponsePaymentMethod deletePaymentMethod(Long id) throws BadRequestExceptions {
        try{
            PaymentMethod record = paymentMethodRepository.findById(id).get();
            paymentMethodRepository.save(PaymentMethod.builder().name(record.getName()).dateRegistration(new Date(System.currentTimeMillis())).id(record.getId()).status(false).build());
            return ResponsePaymentMethod.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public PaymentMethodDTO updatePaymentMethod(String name,Long id) throws BadRequestExceptions {
        try{
            PaymentMethod paymentMethod = paymentMethodRepository.save(PaymentMethod.builder().id(id).dateRegistration(new Date(System.currentTimeMillis())).name(name).status(true).build());
            return PaymentMethodMapper.INSTANCE.paymentMethodToPaymentMethodDTO(paymentMethod);
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }
}
