package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReturnDTO;
import com.proyect.masterdata.dto.StockReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IStockReturnItem {
    StockReturnItem save(StockReturn stockReturn, PurchaseItem purchaseItem, RequestStockReturnItem requestStockReturnItem, User user) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<StockReturnItem> saveAsync(StockReturn stockReturn, PurchaseItem purchaseItem, RequestStockReturnItem requestStockReturnItem, User user) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<StockReturnItemDTO>> list(
            String user,
            List<String> stockReturns,
            List<String> purchases,
            List<String> suppliers,
            List<String> supplierProducts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<StockReturnItemDTO>> listStockReturnItem(String user,Long id) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<List<StockReturnItemDTO>> listStockReturnItemFalse(String user,Long id) throws InternalErrorExceptions,BadRequestExceptions;
}
