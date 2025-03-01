package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderContacted;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OrderContactedRepository;
import com.proyect.masterdata.repository.OrderingRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderContacted;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderContactedImpl implements IOrderContacted {
    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final OrderContactedRepository orderContactedRepository;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<OrderContacted> save(UUID orderId, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderContacted orderContacted;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                orderContacted = orderContactedRepository.findByOrderId(orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(ordering==null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }
            if(orderContacted!=null){
                throw new BadRequestExceptions(Constants.ErrorOrderContactedExists);
            }
            try{
                OrderContacted newOrderContacted = orderContactedRepository.save(OrderContacted.builder()
                                .orderId(ordering.getId())
                                .ordering(ordering)
                                .contacted(false)
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .user(user)
                                .userId(user.getId())
                                .client(user.getClient())
                                .clientId(user.getClientId())
                        .build());
                iAudit.save(
                        "ADD_ORDER_CONTACTED",
                        "PEDIDO "+
                                newOrderContacted.getOrdering().getOrderNumber()+
                                " AGREGADO A CONTACT CENTER.",
                        newOrderContacted.getOrdering().getOrderNumber().toString(),
                        user.getUsername());
                return newOrderContacted;
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> markContacted(UUID orderId, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderContacted orderContacted;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderContacted = orderContactedRepository.findByOrderId(orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderContacted==null){
                throw new BadRequestExceptions(Constants.ErrorOrderContacted);
            }
            try{
                orderContacted.setContacted(true);
                orderContacted.setUpdateDate(OffsetDateTime.now());
                orderContacted.setUser(user);
                orderContacted.setUserId(user.getId());
                orderContacted.setClient(user.getClient());
                orderContacted.setClientId(user.getClientId());
                iAudit.save(
                        "ADD_ORDER_CONTACTED",
                        "PEDIDO "+
                                orderContacted.getOrdering().getOrderNumber()+
                                " AGREGADO A CONTACT CENTER.",
                        orderContacted.getOrdering().getOrderNumber().toString(),
                        user.getUsername());
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
