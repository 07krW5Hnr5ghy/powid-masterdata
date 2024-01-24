package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.domain.PaymentState;
import com.proyect.masterdata.domain.SaleChannel;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestSale;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PaymentMethodRepository;
import com.proyect.masterdata.repository.PaymentStateRepository;
import com.proyect.masterdata.repository.SaleChannelRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISale;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class SaleImpl implements ISale {

    private final UserRepository userRepository;
    private final PaymentStateRepository paymentStateRepository;
    private final PaymentMethodRepository paymentMethodRepository;
    private final SaleChannelRepository saleChannelRepository;
    @Override
    public ResponseSuccess save(RequestSale requestSale, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        PaymentState paymentState;
        PaymentMethod paymentMethod;
        SaleChannel saleChannel;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            paymentState = paymentStateRepository.findByNameAndStatusTrue("POR RECAUDAR");
            paymentMethod = paymentMethodRepository.findByNameAndStatusTrue(requestSale.getPaymentMethod().toUpperCase());
            saleChannel = saleChannelRepository.findByNameAndStatusTrue(requestSale.getSaleChannel().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(paymentMethod == null){
            throw new BadRequestExceptions(Constants.ErrorPaymentMethod);
        }

        if(saleChannel == null){
            throw new BadRequestExceptions(Constants.ErrorSaleChannel);
        }
        return null;
    }
}
