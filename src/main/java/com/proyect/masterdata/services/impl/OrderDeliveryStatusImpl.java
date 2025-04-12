package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderDeliveryStatus;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.OrderDeliveryStatusDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OrderDeliveryStatusRepository;
import com.proyect.masterdata.repository.OrderDeliveryStatusRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderDeliveryStatus;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderDeliveryStatusImpl implements IOrderDeliveryStatus {
    private final UserRepository userRepository;
    private final OrderDeliveryStatusRepository orderDeliveryStatusRepository;
    private final IAudit iAudit;
    private final OrderDeliveryStatusRepositoryCustom orderDeliveryStatusRepositoryCustom;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderDeliveryStatus orderDeliveryStatus;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                orderDeliveryStatus = orderDeliveryStatusRepository.findByNameAndClientId(name.toUpperCase(),user.getClientId());
            }
            if (orderDeliveryStatus != null) {
                throw new BadRequestExceptions(Constants.ErrorOrderDeliveryStatusExists);
            }

            try {
                OrderDeliveryStatus newOrderDeliveryStatus = orderDeliveryStatusRepository.save(OrderDeliveryStatus.builder()
                        .name(name.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .status(true)
                        .user(user)
                        .userId(user.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .build());
                iAudit.save("ADD_ORDER_DELIVERY_STATUS","ESTADO DE ENTREGA "+newOrderDeliveryStatus.getName()+" CREADO.",newOrderDeliveryStatus.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderDeliveryStatus orderDeliveryStatus;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                orderDeliveryStatus = orderDeliveryStatusRepository.findByNameAndClientIdAndStatusTrue(name.toUpperCase(),user.getClientId());
            }
            if (orderDeliveryStatus == null) {
                throw new BadRequestExceptions(Constants.ErrorOrderDeliveryStatus);
            }

            try {
                orderDeliveryStatus.setStatus(false);
                orderDeliveryStatus.setUpdateDate(OffsetDateTime.now());
                orderDeliveryStatus.setUser(user);
                orderDeliveryStatus.setUserId(user.getId());
                orderDeliveryStatusRepository.save(orderDeliveryStatus);
                iAudit.save("DELETE_ORDER_DELIVERY_STATUS","ESTADO DE ENTREGA "+orderDeliveryStatus.getName()+" DESACTIVADO.", orderDeliveryStatus.getName(), user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderDeliveryStatus orderDeliveryStatus;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                orderDeliveryStatus = orderDeliveryStatusRepository.findByNameAndClientIdAndStatusFalse(name.toUpperCase(),user.getClientId());
            }
            if (orderDeliveryStatus == null) {
                throw new BadRequestExceptions(Constants.ErrorOrderDeliveryStatus);
            }

            try {
                orderDeliveryStatus.setStatus(true);
                orderDeliveryStatus.setUpdateDate(OffsetDateTime.now());
                orderDeliveryStatus.setUser(user);
                orderDeliveryStatus.setUserId(user.getId());
                orderDeliveryStatusRepository.save(orderDeliveryStatus);
                iAudit.save("ACTIVATE_ORDER_DELIVERY_STATUS","ESTADO DE ENTREGA "+orderDeliveryStatus.getName()+" ACTIVADO.",orderDeliveryStatus.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<OrderDeliveryStatusDTO>> listOrderDeliveryStatus(String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderDeliveryStatus> orderDeliveryStatuses;
            User user;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderDeliveryStatuses = orderDeliveryStatusRepository.findAllByStatusTrueAndClientId(user.getClientId());
            }
            if (orderDeliveryStatuses.isEmpty()) {
                return Collections.emptyList();
            }
            return orderDeliveryStatuses.stream().map(orderDeliveryStatus -> OrderDeliveryStatusDTO.builder()
                    .id(orderDeliveryStatus.getId())
                    .updateDate(orderDeliveryStatus.getUpdateDate())
                    .name(orderDeliveryStatus.getName())
                    .registrationDate(orderDeliveryStatus.getRegistrationDate())
                    .user(orderDeliveryStatus.getUser().getUsername())
                    .status(orderDeliveryStatus.getStatus())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<OrderDeliveryStatusDTO>> list(String name, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderDeliveryStatus> orderDeliveryStatuses;
            try {
                orderDeliveryStatuses = orderDeliveryStatusRepositoryCustom.searchForOrderDeliveryStatus(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        status);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (orderDeliveryStatuses.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<OrderDeliveryStatusDTO> orderDeliveryStatusDTOS = orderDeliveryStatuses.getContent().stream().map(orderDeliveryStatus -> OrderDeliveryStatusDTO.builder()
                    .id(orderDeliveryStatus.getId())
                    .updateDate(orderDeliveryStatus.getUpdateDate())
                    .name(orderDeliveryStatus.getName())
                    .registrationDate(orderDeliveryStatus.getRegistrationDate())
                    .user(orderDeliveryStatus.getUser().getUsername())
                    .status(orderDeliveryStatus.getStatus())
                    .build()).toList();
            return new PageImpl<>(orderDeliveryStatusDTOS,
                    orderDeliveryStatuses.getPageable(), orderDeliveryStatuses.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<OrderDeliveryStatusDTO>> listFilter(String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderDeliveryStatus> orderDeliveryStatuses;
            User user;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderDeliveryStatuses = orderDeliveryStatusRepository.findAllByClientId(user.getClientId());
            }
            if (orderDeliveryStatuses.isEmpty()) {
                return Collections.emptyList();
            }
            return orderDeliveryStatuses.stream().map(orderDeliveryStatus -> OrderDeliveryStatusDTO.builder()
                    .id(orderDeliveryStatus.getId())
                    .updateDate(orderDeliveryStatus.getUpdateDate())
                    .name(orderDeliveryStatus.getName())
                    .registrationDate(orderDeliveryStatus.getRegistrationDate())
                    .user(orderDeliveryStatus.getUser().getUsername())
                    .status(orderDeliveryStatus.getStatus())
                    .build()).toList();
        });
    }
}
