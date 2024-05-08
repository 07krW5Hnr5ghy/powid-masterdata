package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderStockDTO;
import com.proyect.masterdata.dto.OrderStockItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IOrderStock {
    ResponseSuccess save(Long orderId, String warehouse, List<RequestOrderStockItem> requestOrderStockItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(Long orderId, String warehouse, List<RequestOrderStockItem> requestOrderStockItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<OrderStockDTO>> list(String warehouse, Long orderId, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<OrderStockDTO>> listOrderStock(String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<OrderStockDTO>> listOrderStockFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
