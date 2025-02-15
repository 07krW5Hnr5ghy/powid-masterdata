package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.OrderStockItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IOrderStockItem {
    CompletableFuture<ResponseSuccess> save(UUID orderId, RequestOrderStockItem requestOrderStockItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<OrderStockItemDTO>> list(
            String user,
            UUID orderId,
            List<String> warehouses,
            String productSku,
            String serial,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<Page<OrderStockItemDTO>> listFalse(
            String user,
            UUID orderId,
            List<String> warehouses,
            String productSku,
            String serial,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<Boolean> checkWarehouseItemStock(UUID orderId, Warehouse warehouse, RequestOrderStockItem requestOrderStockItem) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<List<OrderStockItemDTO>> listOrderStockItem(String user,UUID orderId) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<OrderStockItemDTO>> listOrderStockItemFalse(String user,UUID orderId) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(UUID orderId,String supplierProductSerial,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(UUID orderId,String supplierProductSerial,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> update(UUID orderId,String supplierProductSerial,String tokenUser,Integer quantity) throws BadRequestExceptions,InternalErrorExceptions;

}
