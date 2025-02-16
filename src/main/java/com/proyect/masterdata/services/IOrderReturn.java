package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.OrderReturn;
import com.proyect.masterdata.dto.OrderReturnDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IOrderReturn {
    ResponseSuccess save(UUID orderId, List<RequestOrderReturnItem> requestOrderReturnItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(UUID orderId, List<RequestOrderReturnItem> requestOrderReturnItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<OrderReturnDTO>> list(String user) throws BadRequestExceptions;
    CompletableFuture<Page<OrderReturnDTO>> listPagination(
            String user,
            List<UUID> orders,
            List<String> warehouses,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    ) throws BadRequestExceptions;
    CompletableFuture<Page<OrderReturnDTO>> listFalse(
            String user,
            List<UUID> orders,
            List<String> warehouses,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    ) throws BadRequestExceptions;
    CompletableFuture<List<OrderReturnDTO>> listFilter(String user) throws BadRequestExceptions;
    OrderReturn getOrderReturnItemByOrderIdAndClientId(UUID orderId,UUID clientId);
}
