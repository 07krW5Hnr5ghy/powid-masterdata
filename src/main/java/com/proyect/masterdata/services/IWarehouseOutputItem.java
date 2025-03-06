package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.WarehouseOutput;
import com.proyect.masterdata.domain.WarehouseOutputItem;
import com.proyect.masterdata.dto.WarehouseOutputItemDTO;
import com.proyect.masterdata.dto.request.RequestWarehouseOutputItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IWarehouseOutputItem {
    WarehouseOutputItem save(RequestWarehouseOutputItem requestWarehouseOutputItem, WarehouseOutput warehouseOutput, User user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> add(RequestWarehouseOutputItem requestWarehouseOutputItem,UUID warehouseOutputId,String user) throws BadRequestExceptions;
    CompletableFuture<Page<WarehouseOutputItemDTO>> list(
            String user,
            Long orderNumber,
            String ref,
            String warehouse,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
    CompletableFuture<ResponseDelete> delete(UUID productId, UUID warehouseOutputId, String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(UUID productId, UUID warehouseOutputId, String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> updateQuantity(Integer quantity,UUID productId, UUID warehouseOutputId, String user) throws BadRequestExceptions,InternalErrorExceptions;
}
