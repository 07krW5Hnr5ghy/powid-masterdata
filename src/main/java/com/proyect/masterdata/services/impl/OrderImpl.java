package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Order;
import com.proyect.masterdata.domain.OrderState;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestCustomer;
import com.proyect.masterdata.dto.request.RequestItem;
import com.proyect.masterdata.dto.request.RequestOrder;
import com.proyect.masterdata.dto.request.RequestSale;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OrderRepository;
import com.proyect.masterdata.repository.OrderStateRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICustomer;
import com.proyect.masterdata.services.IItem;
import com.proyect.masterdata.services.IOrder;
import com.proyect.masterdata.services.ISale;
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
    private final ISale iSale;
    private final ICustomer iCustomer;
    private final IItem iItem;

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

            Order order = orderRepository.save(Order.builder()
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

            iSale.save(requestOrder.getRequestSale(),tokenUser);
            iCustomer.save(requestOrder.getRequestCustomer(),tokenUser);
            for(RequestItem requestItem : requestOrder.getRequestItems()){
                iItem.save(requestItem,tokenUser);
            }
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
