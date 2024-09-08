package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.dto.OrderItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderItem;
import com.proyect.masterdata.dto.response.ResponseCheckStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IOrderItem {
    ResponseSuccess save(Ordering ordering, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(Ordering ordering, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseCheckStockItem> checkStock(String productSku,Integer quantity,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(Long orderId,String productSku,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> add(Long orderId,RequestOrderItem requestOrderItem,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> update(Long orderId,RequestOrderItem requestOrderItem,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<Page<OrderItemDTO>> listOrderItems(
            String user,
            Long orderId,
            String productSku,
            List<String> colors,
            List<String> sizes,
            List<String> categories,
            Integer quantity,
            Double discount,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<OrderItemDTO>> listByOrder(String user,Long orderId) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<OrderItemDTO>> listByOrderFalse(String user,Long orderId) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseDelete> activate(Long orderId,String productSku,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
