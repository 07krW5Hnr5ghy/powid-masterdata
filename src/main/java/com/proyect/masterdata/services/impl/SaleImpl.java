package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestSale;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.ISale;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;

@Service
@RequiredArgsConstructor
@Log4j2
public class SaleImpl implements ISale {

    private final UserRepository userRepository;
    private final PaymentStateRepository paymentStateRepository;
    private final PaymentMethodRepository paymentMethodRepository;
    private final SaleChannelRepository saleChannelRepository;
    private final ManagementTypeRepository managementTypeRepository;
    private final SaleRepository saleRepository;
    @Override
    public ResponseSuccess save(Ordering ordering, RequestSale requestSale, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        PaymentState paymentState;
        PaymentMethod paymentMethod;
        SaleChannel saleChannel;
        ManagementType managementType;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            paymentState = paymentStateRepository.findByNameAndStatusTrue("POR RECAUDAR");
            paymentMethod = paymentMethodRepository.findByNameAndStatusTrue(requestSale.getPaymentMethod().toUpperCase());
            saleChannel = saleChannelRepository.findByNameAndStatusTrue(requestSale.getSaleChannel().toUpperCase());
            managementType = managementTypeRepository.findByNameAndStatusTrue(requestSale.getManagementType().toUpperCase());
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

        if(managementType == null){
            throw new BadRequestExceptions(Constants.ErrorManagementType);
        }
        
        try{
            Double duePayment = (requestSale.getSaleAmount() + requestSale.getDeliveryAmount()) - requestSale.getAdvancedPayment();
            saleRepository.save(Sale.builder()
                            .tokenUser(user.getUsername())
                            .advancePayment(requestSale.getAdvancedPayment())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .deliveryAddress(requestSale.getDeliveryAddress().toUpperCase())
                            .ordering(ordering)
                            .orderId(ordering.getId())
                            .duePayment(duePayment)
                            .managementType(managementType)
                            .managementTypeId(managementType.getId())
                            .observations(requestSale.getObservations().toUpperCase())
                            .paymentMethod(paymentMethod)
                            .paymentMethodId(paymentMethod.getId())
                            .saleAmount(requestSale.getSaleAmount())
                            .deliveryAmount(requestSale.getDeliveryAmount())
                            .paymentState(paymentState)
                            .paymentStateId(paymentState.getId())
                            .saleChannel(saleChannel)
                            .saleChannelId(saleChannel.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .seller(requestSale.getSeller().toUpperCase())
                            .tokenUser(user.getUsername())
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
