package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderState;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.OrderStateDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.OrderStateMapper;
import com.proyect.masterdata.repository.OrderStateRepository;
import com.proyect.masterdata.repository.OrderStateRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderState;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderStateImpl implements IOrderState {
    private final OrderStateRepository orderStateRepository;
    private final UserRepository userRepository;
    private final OrderStateRepositoryCustom orderStateRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name,String hexColor, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderState orderState;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderState != null) {
                throw new BadRequestExceptions(Constants.ErrorOrderStateExist.toUpperCase());
            }

            try {
                OrderState newOrderState = orderStateRepository.save(OrderState.builder()
                                .name(name.toUpperCase())
                                .hexColor(hexColor)
                                .status(true)
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .user(user)
                                .userId(user.getId())
                        .build());
                iAudit.save("ADD_ORDER_STATE","ESTADO DE PEDIDO "+newOrderState.getName()+" CREADO.",newOrderState.getName(),user.getUsername());
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
    @Transactional
    public CompletableFuture<ResponseDelete> delete(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderState orderState;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderState == null) {
                throw new BadRequestExceptions(Constants.ErrorOrderState.toUpperCase());
            }

            try {
                orderState.setStatus(false);
                orderState.setRegistrationDate(OffsetDateTime.now());
                orderState.setUser(user);
                orderState.setUserId(user.getId());
                orderStateRepository.save(orderState);
                iAudit.save("DELETE_ORDER_STATE","ESTADO DE PEDIDO "+orderState.getName()+" DESACTIVADO.",orderState.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderState orderState;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderState == null) {
                throw new BadRequestExceptions(Constants.ErrorOrderState.toUpperCase());
            }

            try {
                orderState.setStatus(true);
                orderState.setRegistrationDate(OffsetDateTime.now());
                orderState.setUser(user);
                orderState.setUserId(user.getId());
                orderStateRepository.save(orderState);
                iAudit.save("ACTIVATE_ORDER_STATE","ESTADO DE PEDIDO "+orderState.getName()+" ACTIVADO.",orderState.getName(),user.getUsername());
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
    public CompletableFuture<List<OrderStateDTO>> listState() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderState> states;
            try {
                states = orderStateRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (states.isEmpty()) {
                return Collections.emptyList();
            }
            return states.stream().map(orderState -> OrderStateDTO.builder()
                    .name(orderState.getName())
                    .hexColor(orderState.getHexColor())
                    .id(orderState.getId())
                    .status(orderState.getStatus())
                    .user(orderState.getUser().getUsername())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<OrderStateDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
                                    Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderState> statePage;
            try {
                statePage = orderStateRepositoryCustom.searchForOrderState(name, user, sort, sortColumn, pageNumber,
                        pageSize, true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (statePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<OrderStateDTO> orderStateDTOS = statePage.getContent().stream().map(orderState -> OrderStateDTO.builder()
                    .name(orderState.getName())
                    .hexColor(orderState.getHexColor())
                    .id(orderState.getId())
                    .status(orderState.getStatus())
                    .user(orderState.getUser().getUsername())
                    .build()).toList();
            return new PageImpl<>(orderStateDTOS,
                    statePage.getPageable(), statePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<OrderStateDTO>> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
                                               Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderState> statePage;
            try {
                statePage = orderStateRepositoryCustom.searchForOrderState(name, user, sort, sortColumn, pageNumber,
                        pageSize, false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (statePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<OrderStateDTO> orderStateDTOS = statePage.getContent().stream().map(orderState -> OrderStateDTO.builder()
                    .name(orderState.getName())
                    .hexColor(orderState.getHexColor())
                    .id(orderState.getId())
                    .status(orderState.getStatus())
                    .user(orderState.getUser().getUsername())
                    .build()).toList();
            return new PageImpl<>(orderStateDTOS,
                    statePage.getPageable(), statePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<OrderStateDTO>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderState> states;
            try {
                states = orderStateRepository.findAll();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (states.isEmpty()) {
                return Collections.emptyList();
            }
            return states.stream().map(orderState -> OrderStateDTO.builder()
                    .name(orderState.getName())
                    .hexColor(orderState.getHexColor())
                    .id(orderState.getId())
                    .status(orderState.getStatus())
                    .user(orderState.getUser().getUsername())
                    .build()).toList();
        });
    }
}
