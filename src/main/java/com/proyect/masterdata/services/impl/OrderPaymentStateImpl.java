package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderPaymentState;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.OrderPaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestOrderPaymentState;
import com.proyect.masterdata.dto.request.RequestOrderPaymentStateSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.PaymentStateMapper;
import com.proyect.masterdata.repository.OrderPaymentStateRepository;
import com.proyect.masterdata.repository.OrderPaymentStateRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderPaymentState;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderPaymentStateImpl implements IOrderPaymentState {

    private final OrderPaymentStateRepository orderPaymentStateRepository;
    private final PaymentStateMapper paymentStateMapper;
    private final UserRepository userRepository;
    private final OrderPaymentStateRepositoryCustom orderPaymentStateRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderPaymentState orderPaymentState;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderPaymentState = orderPaymentStateRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderPaymentState != null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentStateExists.toUpperCase());
            }

            try {
                OrderPaymentState newOrderPaymentState = orderPaymentStateRepository.save(OrderPaymentState.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .user(user)
                                .userId(user.getId())
                        .build());
                iAudit.save("ADD_ORDER_PAYMENT_STATE","ESTADO DE PAGO DE PEDIDO "+newOrderPaymentState.getName()+" CREADO.",newOrderPaymentState.getName(), user.getUsername());
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
            OrderPaymentState orderPaymentState;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderPaymentState = orderPaymentStateRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderPaymentState == null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentState.toUpperCase());
            }

            try {
                orderPaymentState.setStatus(false);
                orderPaymentState.setUpdateDate(OffsetDateTime.now());
                orderPaymentState.setUser(user);
                orderPaymentState.setUserId(user.getId());
                orderPaymentStateRepository.save(orderPaymentState);
                iAudit.save("DELETE_ORDER_PAYMENT_STATE","ESTADO DE PAGO DE PEDIDO "+orderPaymentState.getName()+" DESACTIVADO.",orderPaymentState.getName(), user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderPaymentState orderPaymentState;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderPaymentState = orderPaymentStateRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderPaymentState == null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentState.toUpperCase());
            }

            try {
                orderPaymentState.setStatus(true);
                orderPaymentState.setUpdateDate(OffsetDateTime.now());
                orderPaymentState.setUser(user);
                orderPaymentState.setUserId(user.getId());
                orderPaymentStateRepository.save(orderPaymentState);
                iAudit.save("ACTIVATE_ORDER_PAYMENT_STATE","ESTADO DE PAGO DE PEDIDO "+orderPaymentState.getName()+" ACTIVADO.",orderPaymentState.getName(), user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
            }
        });
    }

    @Override
    public CompletableFuture<Page<OrderPaymentStateDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
                                           Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderPaymentState> paymentStatePage;
            try {
                paymentStatePage = orderPaymentStateRepositoryCustom.searchForPaymentState(name, user, sort, sortColumn,
                        pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (paymentStatePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(paymentStateMapper.listPaymentStateToListPaymentStateDTO(paymentStatePage.getContent()),
                    paymentStatePage.getPageable(), paymentStatePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<OrderPaymentStateDTO>> listPaymentState() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderPaymentState> orderPaymentStates = new ArrayList<>();
            try {
                orderPaymentStates = orderPaymentStateRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (orderPaymentStates.isEmpty()) {
                return Collections.emptyList();
            }
            return paymentStateMapper.listPaymentStateToListPaymentStateDTO(orderPaymentStates);
        });
    }

    @Override
    public CompletableFuture<Page<OrderPaymentStateDTO>> listStatusFalse(String name, String user, String sort, String sortColumn,
                                                      Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderPaymentState> paymentStatePage;
            try {
                paymentStatePage = orderPaymentStateRepositoryCustom.searchForPaymentState(name, user, sort, sortColumn,
                        pageNumber, pageSize, false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (paymentStatePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(paymentStateMapper.listPaymentStateToListPaymentStateDTO(paymentStatePage.getContent()),
                    paymentStatePage.getPageable(), paymentStatePage.getTotalElements());
        });
    }

}
