package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IOrderReturnItem {
    CompletableFuture<ResponseSuccess> save(UUID orderId, RequestOrderReturnItem requestOrderReturnItem, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(UUID orderId,UUID supplierProductId,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> activate(UUID orderId,UUID supplierProductId,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> update(UUID orderId,UUID supplierProductId,Integer quantity,String tokenUser) throws InternalErrorExceptions;
    CompletableFuture<List<OrderReturnItemDTO>> list(String user,UUID orderId) throws BadRequestExceptions;
    CompletableFuture<Page<OrderReturnItemDTO>> listPagination(
            String user,
            List<UUID> orderIds,
            List<UUID> productIds,
            List<UUID> supplierProductIds,
            List<String> warehouses,
            List<String> orderReturnTypes,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    ) throws BadRequestExceptions;
    CompletableFuture<Page<OrderReturnItemDTO>> listFalse(
            String user,
            List<UUID> orderIds,
            List<UUID> productIds,
            List<UUID> supplierProductIds,
            List<String> warehouses,
            List<String> orderReturnTypes,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    ) throws BadRequestExceptions;
}
