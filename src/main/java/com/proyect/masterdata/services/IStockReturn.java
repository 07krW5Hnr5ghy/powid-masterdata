package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.StockReturnDTO;
import com.proyect.masterdata.dto.request.RequestStockReturn;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IStockReturn {
    ResponseSuccess save(RequestStockReturn requestStockReturn) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestStockReturn requestStockReturn) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<StockReturnDTO>> list(
            String user,
            List<String> serials,
            List<String> purchases,
            List<String> suppliers,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<StockReturnDTO>> listStockReturn(String user) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<List<StockReturnDTO>> listStockReturnFalse(String user) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<List<StockReturnDTO>> listFilter(String user) throws InternalErrorExceptions,BadRequestExceptions;
}
