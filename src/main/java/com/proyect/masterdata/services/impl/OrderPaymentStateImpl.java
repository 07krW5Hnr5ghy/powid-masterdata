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
import com.proyect.masterdata.services.IOrderPaymentState;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderPaymentStateImpl implements IOrderPaymentState {

    private final OrderPaymentStateRepository orderPaymentStateRepository;
    private final PaymentStateMapper paymentStateMapper;
    private final UserRepository userRepository;
    private final OrderPaymentStateRepositoryCustom orderPaymentStateRepositoryCustom;

    @Override
    public ResponseSuccess save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        OrderPaymentState orderPaymentState;

        try {
            datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
            orderPaymentState = orderPaymentStateRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (orderPaymentState != null) {
            throw new BadRequestExceptions(Constants.ErrorPaymentStateExists.toUpperCase());
        }

        try {
            orderPaymentStateRepository.save(OrderPaymentState.builder()
                            .name(name.toUpperCase())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.toUpperCase())
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names, String user)
            throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        List<OrderPaymentState> orderPaymentStates;

        try {
            datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
            orderPaymentStates = orderPaymentStateRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (!orderPaymentStates.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorPaymentStateList.toUpperCase());
        }

        try {
            List<RequestOrderPaymentStateSave> requestOrderPaymentStateSaves = names.stream()
                    .map(data -> RequestOrderPaymentStateSave.builder()
                            .user(user.toUpperCase())
                            .name(data.toUpperCase())
                            .build())
                    .toList();
            orderPaymentStateRepository.saveAll(paymentStateMapper.listPaymentStateToListName(requestOrderPaymentStateSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        OrderPaymentState orderPaymentState;

        try {
            datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
            orderPaymentState = orderPaymentStateRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (orderPaymentState == null) {
            throw new BadRequestExceptions(Constants.ErrorPaymentState.toUpperCase());
        }

        try {
            orderPaymentState.setStatus(false);
            orderPaymentState.setUpdateDate(new Date(System.currentTimeMillis()));
            orderPaymentStateRepository.save(orderPaymentState);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public Page<OrderPaymentStateDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
                                           Integer pageSize) throws BadRequestExceptions {
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
    }

    @Override
    public List<OrderPaymentStateDTO> listPaymentState() throws BadRequestExceptions {
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
    }

    @Override
    public Page<OrderPaymentStateDTO> listStatusFalse(String name, String user, String sort, String sortColumn,
                                                      Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
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
    }

}
