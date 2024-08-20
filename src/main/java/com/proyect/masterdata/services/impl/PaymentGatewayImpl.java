package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentGateway;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PaymentGatewayRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IPaymentGateway;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class PaymentGatewayImpl implements IPaymentGateway {
    private final PaymentGatewayRepository paymentGatewayRepository;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                PaymentGateway newPaymentGateway = paymentGatewayRepository.save(PaymentGateway.builder()
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .name(name.toUpperCase())
                                .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_PAYMENT_GATEWAY","PASARELA DE PAGO "+newPaymentGateway.getName()+" CREADA.",newPaymentGateway.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            PaymentGateway paymentGateway;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                paymentGateway = paymentGatewayRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(paymentGateway == null){
                throw new BadRequestExceptions(Constants.ErrorPaymentGateway);
            }

            try{
                paymentGateway.setStatus(false);
                paymentGateway.setUpdateDate(new Date(System.currentTimeMillis()));
                paymentGateway.setTokenUser(user.getUsername());
                iAudit.save("DELETE_PAYMENT_GATEWAY","PASARELA DE PAGO "+paymentGateway.getName()+" DESACTIVADA.",paymentGateway.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            PaymentGateway paymentGateway;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                paymentGateway = paymentGatewayRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(paymentGateway == null){
                throw new BadRequestExceptions(Constants.ErrorPaymentGateway);
            }

            try{
                paymentGateway.setStatus(true);
                paymentGateway.setUpdateDate(new Date(System.currentTimeMillis()));
                paymentGateway.setTokenUser(user.getUsername());
                iAudit.save("ACTIVATE_PAYMENT_GATEWAY","PASARELA DE PAGO "+paymentGateway.getName()+" ACTIVADA.",paymentGateway.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
