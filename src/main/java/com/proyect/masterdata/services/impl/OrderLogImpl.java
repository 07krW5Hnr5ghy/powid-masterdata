package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderLog;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.OrderLogDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OrderLogRepository;
import com.proyect.masterdata.services.IOrderLog;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderLogImpl implements IOrderLog {
    private final OrderLogRepository orderLogRepository;
    @Override
    public OrderLog save(User user, Ordering order,String detail) throws InternalErrorExceptions, BadRequestExceptions {
        try{
            return orderLogRepository.save(OrderLog.builder()
                            .ordering(order)
                            .orderId(order.getId())
                            .orderState(order.getOrderState())
                            .orderStateId(order.getOrderStateId())
                            .detail(detail)
                            .user(user)
                            .userId(user.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                    .build());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<OrderLogDTO> listLogByOrder(UUID orderId) throws InternalErrorExceptions, BadRequestExceptions {
        List<OrderLog> orderLogList;
        try{
            orderLogList = orderLogRepository.findAllByOrderId(orderId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(orderLogList.isEmpty()){
            return new ArrayList<>();
        }
        try{
            return orderLogList.stream().map(orderLog -> OrderLogDTO.builder()
                    .id(orderLog.getId())
                    .user(orderLog.getUser().getUsername())
                    .userFullName(orderLog.getUser().getName()+" "+orderLog.getUser().getSurname())
                    .orderState(orderLog.getOrderState().getName())
                    .registrationDate(orderLog.getRegistrationDate())
                    .detail(orderLog.getDetail())
                    .build()).toList();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
