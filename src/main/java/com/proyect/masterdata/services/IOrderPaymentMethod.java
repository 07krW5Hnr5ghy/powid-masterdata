package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderPaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestOrderPaymentMethod;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IOrderPaymentMethod {
    CompletableFuture<ResponseSuccess> save(String name, String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(String name,String user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(String name,String user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<OrderPaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions;
    CompletableFuture<Page<OrderPaymentMethodDTO>> list(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) throws BadRequestExceptions;
    CompletableFuture<List<OrderPaymentMethodDTO>> listFilter() throws BadRequestExceptions;
}
