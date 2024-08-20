package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderReturnType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.OrderReturnTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderReturnType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderReturnTypeImpl implements IOrderReturnType {
    private final OrderReturnTypeRepository orderReturnTypeRepository;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderReturnType orderReturnType;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                orderReturnType = orderReturnTypeRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderReturnType != null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnType);
            }
            try {
                OrderReturnType newOrderReturnType = orderReturnTypeRepository.save(OrderReturnType.builder()
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .status(true)
                                .tokenUser(user.getUsername())
                        .name(name.toUpperCase())
                        .build());
                iAudit.save("ADD_ORDER_RETURN_TYPE","TIPO DE DEVOLUCION DE PEDIDO "+newOrderReturnType.getName()+" CREADO.",newOrderReturnType.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<String>> list() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderReturnType> orderReturnTypeList;
            try {
                orderReturnTypeList = orderReturnTypeRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderReturnTypeList.isEmpty()){
                return Collections.emptyList();
            }
            return orderReturnTypeList.stream().map(OrderReturnType::getName).toList();
        });
    }

    @Override
    public CompletableFuture<List<String>> listFalse() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderReturnType> orderReturnTypeList;
            try {
                orderReturnTypeList = orderReturnTypeRepository.findAllByStatusFalse();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderReturnTypeList.isEmpty()){
                return Collections.emptyList();
            }
            return orderReturnTypeList.stream().map(OrderReturnType::getName).toList();
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            OrderReturnType orderReturnType;
            User user;
            try{
                user = userRepository.findByUsernameAndStatusTrue(name.toUpperCase());
                orderReturnType = orderReturnTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderReturnType == null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnType);
            }
            try {
                orderReturnType.setStatus(false);
                orderReturnType.setUpdateDate(new Date(System.currentTimeMillis()));
                orderReturnType.setTokenUser(user.getUsername());
                iAudit.save("DELETE_ORDER_RETURN_TYPE","TIPO DE DEVOLUCION DE PEDIDO "+orderReturnType.getName()+" DESACTIVADO.",orderReturnType.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            OrderReturnType orderReturnType;
            User user;
            try{
                user = userRepository.findByUsernameAndStatusTrue(name.toUpperCase());
                orderReturnType = orderReturnTypeRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderReturnType == null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnType);
            }
            try {
                orderReturnType.setStatus(true);
                orderReturnType.setUpdateDate(new Date(System.currentTimeMillis()));
                orderReturnType.setTokenUser(user.getUsername());
                iAudit.save("ACTIVATE_ORDER_RETURN_TYPE","TIPO DE DEVOLUCION DE PEDIDO "+orderReturnType.getName()+" ACTIVADO.",orderReturnType.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<String>> listFilter() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderReturnType> orderReturnTypeList;
            try {
                orderReturnTypeList = orderReturnTypeRepository.findAll();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderReturnTypeList.isEmpty()){
                return Collections.emptyList();
            }
            return orderReturnTypeList.stream().map(OrderReturnType::getName).toList();
        });
    }
}
