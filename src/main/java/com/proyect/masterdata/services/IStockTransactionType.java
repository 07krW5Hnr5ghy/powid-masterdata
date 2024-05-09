package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.dto.StockTransactionTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IStockTransactionType {
    ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<StockTransactionTypeDTO>> list() throws BadRequestExceptions;
}