package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.StockTransaction;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.StockTransactionItemDTO;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IStockTransactionItem {
        ResponseSuccess save(StockTransaction stockTransaction,RequestStockTransactionItem requestStockTransactionItem, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(StockTransaction stockTransaction, RequestStockTransactionItem requestStockTransactionItem, String tokenUser)
                throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<StockTransactionItemDTO>> list(String user, String stockTransactionSerial,String supplierProductSerial, String sort, String sortColumn,
                                           Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<List<StockTransactionItemDTO>> listStockTransactionItem(String user,Long id) throws InternalErrorExceptions,BadRequestExceptions;
}
