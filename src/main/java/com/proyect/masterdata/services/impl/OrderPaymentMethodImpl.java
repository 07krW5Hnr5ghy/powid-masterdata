package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderPaymentMethod;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.OrderPaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestOrderPaymentMethod;
import com.proyect.masterdata.dto.request.RequestOrderPaymentMethodSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.PaymentMethodMapper;
import com.proyect.masterdata.repository.OrderPaymentMethodRepository;
import com.proyect.masterdata.repository.OrderPaymentMethodRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderPaymentMethod;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderPaymentMethodImpl implements IOrderPaymentMethod {

    private final OrderPaymentMethodRepository orderPaymentMethodRepository;
    private final PaymentMethodMapper paymentMethodMapper;
    private final UserRepository userRepository;
    private final OrderPaymentMethodRepositoryCustom orderPaymentMethodRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderPaymentMethod orderPaymentMethod;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderPaymentMethod != null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentMethodExists.toUpperCase());
            }

            try {
                OrderPaymentMethod newOrderPaymentMethod = orderPaymentMethodRepository.save(OrderPaymentMethod.builder()
                        .user(user)
                                .userId(user.getId())
                        .status(true)
                        .name(name.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .build());
                iAudit.save("ADD_ORDER_PAYMENT_METHOD","METODO DE PAGO "+newOrderPaymentMethod.getName()+" CREADO.",newOrderPaymentMethod.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    @Transactional
    public CompletableFuture<ResponseDelete> delete(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderPaymentMethod orderPaymentMethod;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderPaymentMethod == null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentMethod.toUpperCase());
            }

            try {
                orderPaymentMethod.setStatus(false);
                orderPaymentMethod.setUpdateDate(OffsetDateTime.now());
                orderPaymentMethod.setUser(user);
                orderPaymentMethod.setUserId(user.getId());
                orderPaymentMethodRepository.save(orderPaymentMethod);
                iAudit.save("DELETE_ORDER_PAYMENT_METHOD","METODO DE PAGO "+orderPaymentMethod.getName()+" DESACTIVADO.",orderPaymentMethod.getName(),user.getUsername());
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
            OrderPaymentMethod orderPaymentMethod;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderPaymentMethod == null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentMethod.toUpperCase());
            }

            try {
                orderPaymentMethod.setStatus(true);
                orderPaymentMethod.setUpdateDate(OffsetDateTime.now());
                orderPaymentMethod.setUser(user);
                orderPaymentMethod.setUserId(user.getId());
                orderPaymentMethodRepository.save(orderPaymentMethod);
                iAudit.save("ACTIVATE_ORDER_PAYMENT_METHOD","METODO DE PAGO "+orderPaymentMethod.getName()+" ACTIVADO.",orderPaymentMethod.getName(),user.getUsername());
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
    public CompletableFuture<List<OrderPaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderPaymentMethod> orderPaymentMethods = new ArrayList<>();
            try {
                orderPaymentMethods = orderPaymentMethodRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (orderPaymentMethods.isEmpty()) {
                return Collections.emptyList();
            }
            return orderPaymentMethods.stream().map(orderPaymentMethod -> OrderPaymentMethodDTO.builder()
                    .id(orderPaymentMethod.getId())
                    .name(orderPaymentMethod.getName())
                    .user(orderPaymentMethod.getUser().getUsername())
                    .status(orderPaymentMethod.getStatus())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<OrderPaymentMethodDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
                                            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderPaymentMethod> orderPaymentMethodPage;
            try {
                orderPaymentMethodPage = orderPaymentMethodRepositoryCustom.searchForPaymentMethod(name, user, sort, sortColumn,
                        pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (orderPaymentMethodPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<OrderPaymentMethodDTO> orderPaymentMethodDTOS = orderPaymentMethodPage.getContent().stream().map(orderPaymentMethod -> OrderPaymentMethodDTO.builder()
                    .id(orderPaymentMethod.getId())
                    .name(orderPaymentMethod.getName())
                    .user(orderPaymentMethod.getUser().getUsername())
                    .status(orderPaymentMethod.getStatus())
                    .build()).toList();

            return new PageImpl<>(
                    orderPaymentMethodDTOS,
                    orderPaymentMethodPage.getPageable(), orderPaymentMethodPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<OrderPaymentMethodDTO>> listStatusFalse(String name, String user, String sort, String sortColumn,
                                                       Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderPaymentMethod> orderPaymentMethodPage;
            try {
                orderPaymentMethodPage = orderPaymentMethodRepositoryCustom.searchForPaymentMethod(name, user, sort, sortColumn,
                        pageNumber, pageSize, false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (orderPaymentMethodPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<OrderPaymentMethodDTO> orderPaymentMethodDTOS = orderPaymentMethodPage.getContent().stream().map(orderPaymentMethod -> OrderPaymentMethodDTO.builder()
                    .id(orderPaymentMethod.getId())
                    .name(orderPaymentMethod.getName())
                    .user(orderPaymentMethod.getUser().getUsername())
                    .status(orderPaymentMethod.getStatus())
                    .build()).toList();

            return new PageImpl<>(
                    orderPaymentMethodDTOS,
                    orderPaymentMethodPage.getPageable(), orderPaymentMethodPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<OrderPaymentMethodDTO>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderPaymentMethod> orderPaymentMethods = new ArrayList<>();
            try {
                orderPaymentMethods = orderPaymentMethodRepository.findAll();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (orderPaymentMethods.isEmpty()) {
                return Collections.emptyList();
            }
            return orderPaymentMethods.stream().map(orderPaymentMethod -> OrderPaymentMethodDTO.builder()
                    .id(orderPaymentMethod.getId())
                    .name(orderPaymentMethod.getName())
                    .user(orderPaymentMethod.getUser().getUsername())
                    .status(orderPaymentMethod.getStatus())
                    .build()).toList();
        });
    }
}
