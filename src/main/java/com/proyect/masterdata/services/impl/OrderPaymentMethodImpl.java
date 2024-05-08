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
import com.proyect.masterdata.services.IOrderPaymentMethod;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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

    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            OrderPaymentMethod orderPaymentMethod;

            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderPaymentMethod != null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentMethodExists.toUpperCase());
            }

            try {
                orderPaymentMethodRepository.save(OrderPaymentMethod.builder()
                        .tokenUser(user.toUpperCase())
                        .status(true)
                        .name(name.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .build());
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
    public ResponseSuccess saveAll(List<String> names, String user)
            throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        List<OrderPaymentMethod> orderPaymentMethods;
        try {
            datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
            orderPaymentMethods = orderPaymentMethodRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (!orderPaymentMethods.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorPaymentMethodList.toUpperCase());
        }

        try {
            List<RequestOrderPaymentMethodSave> paymentMethodSaves = names.stream()
                    .map(data -> RequestOrderPaymentMethodSave.builder()
                            .user(user.toUpperCase())
                            .name(data.toUpperCase())
                            .build())
                    .toList();
            orderPaymentMethodRepository.saveAll(paymentMethodMapper.listPaymentMethodToListName(paymentMethodSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public CompletableFuture<ResponseDelete> delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            OrderPaymentMethod orderPaymentMethod;

            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderPaymentMethod == null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentMethod.toUpperCase());
            }

            try {
                orderPaymentMethod.setStatus(false);
                orderPaymentMethod.setUpdateDate(new Date(System.currentTimeMillis()));
                orderPaymentMethodRepository.save(orderPaymentMethod);
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
            return paymentMethodMapper.listPaymentMethodToListPaymentMethodDTO(orderPaymentMethods);
        });
    }

    @Override
    public CompletableFuture<Page<OrderPaymentMethodDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
                                            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderPaymentMethod> paymentMethodPage;
            try {
                paymentMethodPage = orderPaymentMethodRepositoryCustom.searchForPaymentMethod(name, user, sort, sortColumn,
                        pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (paymentMethodPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            return new PageImpl<>(
                    paymentMethodMapper.listPaymentMethodToListPaymentMethodDTO(paymentMethodPage.getContent()),
                    paymentMethodPage.getPageable(), paymentMethodPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<OrderPaymentMethodDTO>> listStatusFalse(String name, String user, String sort, String sortColumn,
                                                       Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderPaymentMethod> paymentMethodPage;
            try {
                paymentMethodPage = orderPaymentMethodRepositoryCustom.searchForPaymentMethod(name, user, sort, sortColumn,
                        pageNumber, pageSize, false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (paymentMethodPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            return new PageImpl<>(
                    paymentMethodMapper.listPaymentMethodToListPaymentMethodDTO(paymentMethodPage.getContent()),
                    paymentMethodPage.getPageable(), paymentMethodPage.getTotalElements());
        });
    }
}
