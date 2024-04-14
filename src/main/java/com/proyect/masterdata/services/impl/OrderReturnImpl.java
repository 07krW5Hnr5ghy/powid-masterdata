package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderReturn;
import com.proyect.masterdata.domain.OrderStock;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OrderReturnRepository;
import com.proyect.masterdata.repository.OrderStockRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IOrderReturn;
import com.proyect.masterdata.services.IOrderReturnItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderReturnImpl implements IOrderReturn {
    private final OrderReturnRepository orderReturnRepository;
    private final OrderStockRepository orderStockRepository;
    private final UserRepository userRepository;
    private final IOrderReturnItem iOrderReturnItem;
    @Override
    public ResponseSuccess save(Long orderId, List<RequestOrderReturnItem> requestOrderReturnItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        OrderReturn orderReturn;
        OrderStock orderStock;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            orderReturn = orderReturnRepository.findByOrderId(orderId);
            orderStock = orderStockRepository.findByOrderId(orderId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(orderReturn != null){
            throw new BadRequestExceptions(Constants.ErrorOrderReturnExists);
        }
        if(orderStock == null){
            throw new BadRequestExceptions(Constants.ErrorOrderStock);
        }
        try {
            OrderReturn newOrderReturn = orderReturnRepository.save(OrderReturn.builder()
                            .order(orderStock.getOrdering())
                            .orderId(orderStock.getOrderId())
                            .orderStock(orderStock)
                            .orderStockId(orderStock.getId())
                            .tokenUser(user.getUsername())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                    .build());
            for(RequestOrderReturnItem requestOrderReturnItem : requestOrderReturnItemList){
                iOrderReturnItem.save(newOrderReturn.getId(),orderStock.getOrderId(),requestOrderReturnItem,tokenUser);
            }
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
