package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ISupplierType {
    ResponseSuccess save(String name,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<String>> listSupplierType() throws BadRequestExceptions;
}
