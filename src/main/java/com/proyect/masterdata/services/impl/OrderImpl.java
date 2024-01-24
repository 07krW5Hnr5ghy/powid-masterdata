package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Order;
import com.proyect.masterdata.domain.OrderState;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestOrder;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OrderRepository;
import com.proyect.masterdata.repository.OrderStateRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IOrder;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;

@RequiredArgsConstructor
@Service
@Log4j2
public class OrderImpl implements IOrder {

    private final UserRepository userRepository;
    private final OrderRepository orderRepository;
    private final OrderStateRepository orderStateRepository;
    @Override
    public ResponseSuccess save(RequestOrder requestOrder, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        OrderState orderState;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            orderState = orderStateRepository.findByNameAndStatusTrue("PENDIENTE");
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try{
            orderRepository.save(Order.builder()
                            .cancellation(false)
                            .orderState(orderState)
                            .orderStateId(orderState.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .deliveryMan(requestOrder.getDeliveryMan())
                            .deliveryPhone(requestOrder.getDeliveryManPhone())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
