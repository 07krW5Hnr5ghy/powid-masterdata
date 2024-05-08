package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderStateDTO;
import com.proyect.masterdata.dto.request.RequestState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IOrderState {
    CompletableFuture<ResponseSuccess> save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> names, String user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<OrderStateDTO> update(RequestState requestState) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<OrderStateDTO>> listState() throws BadRequestExceptions;
    CompletableFuture<Page<OrderStateDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize)
            throws BadRequestExceptions;
    CompletableFuture<Page<OrderStateDTO>> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
                                        Integer pageSize) throws BadRequestExceptions;
}
