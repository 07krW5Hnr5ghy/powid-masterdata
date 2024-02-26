package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentGateway;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PaymentGatewayRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IPaymentGateway;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;

@Service
@RequiredArgsConstructor
@Log4j2
public class PaymentGatewayImpl implements IPaymentGateway {
    private final PaymentGatewayRepository paymentGatewayRepository;
    private final UserRepository userRepository;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        PaymentGateway paymentGateway;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            paymentGateway = paymentGatewayRepository.findByName(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(paymentGateway != null){
            throw new BadRequestExceptions(Constants.ErrorPaymentGatewayExists);
        }

        try{
            paymentGatewayRepository.save(PaymentGateway.builder()
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .status(true)
                            .name(name.toUpperCase())
                    .build());
            return ResponseSuccess.builder()
                    .message(Constants.register)
                    .code(200)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
