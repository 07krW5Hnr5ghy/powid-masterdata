package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderState;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.OrderStateDTO;
import com.proyect.masterdata.dto.request.RequestState;
import com.proyect.masterdata.dto.request.RequestStateSave;
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

import java.sql.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderStateImpl implements IOrderState {
    private final OrderStateRepository orderStateRepository;
    private final OrderStateMapper stateMapper;
    private final UserRepository userRepository;
    private final OrderStateRepositoryCustom orderStateRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            OrderState orderState;

            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderState != null) {
                throw new BadRequestExceptions(Constants.ErrorStateExist.toUpperCase());
            }

            try {
                OrderState newOrderState = orderStateRepository.save(stateMapper.stateToName(RequestStateSave.builder()
                        .name(name.toUpperCase()).user(datauser.getUsername().toUpperCase()).build()));
                iAudit.save("ADD_ORDER_STATE","ADD ORDER STATE "+newOrderState.getName()+".",datauser.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            OrderState orderState;

            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderState == null) {
                throw new BadRequestExceptions(Constants.ErrorState.toUpperCase());
            }

            try {
                orderState.setStatus(false);
                orderState.setRegistrationDate(new Date(System.currentTimeMillis()));
                orderState.setTokenUser(datauser.getUsername());
                orderStateRepository.save(orderState);
                iAudit.save("DELETE_ORDER_STATE","DELETE ORDER STATE "+orderState.getName()+".",datauser.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            OrderState orderState;

            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (datauser == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (orderState == null) {
                throw new BadRequestExceptions(Constants.ErrorState.toUpperCase());
            }

            try {
                orderState.setStatus(true);
                orderState.setRegistrationDate(new Date(System.currentTimeMillis()));
                orderState.setTokenUser(datauser.getUsername());
                orderStateRepository.save(orderState);
                iAudit.save("ACTIVATE_ORDER_STATE","ACTIVATE ORDER STATE "+orderState.getName()+".",datauser.getUsername());
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
            List<OrderState> states = new ArrayList<>();
            try {
                states = orderStateRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (states.isEmpty()) {
                return Collections.emptyList();
            }
            return stateMapper.listStateToListStateDTO(states);
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
            return new PageImpl<>(stateMapper.listStateToListStateDTO(statePage.getContent()),
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
            return new PageImpl<>(stateMapper.listStateToListStateDTO(statePage.getContent()),
                    statePage.getPageable(), statePage.getTotalElements());
        });
    }
}
