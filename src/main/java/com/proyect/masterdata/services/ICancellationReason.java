package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ICancellationReason {
    ResponseSuccess save(String name,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<String>> list(String name, String sort, String sortColumn, Integer pageNumber,
                                         Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<Page<String>> listFalse(String name, String sort, String sortColumn, Integer pageNumber,
                                              Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String name,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> activate(String name,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
