package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.WarehouseOutputDTO;
import com.proyect.masterdata.dto.request.RequestWarehouseOutput;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IWarehouseOutput {
    CompletableFuture<ResponseSuccess> save(RequestWarehouseOutput requestWarehouseOutput) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseDelete> close(String username, UUID warehouseOutputId) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> reactivate(String username, UUID warehouseOutputId) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<WarehouseOutputDTO>> list(
            String username,
            Long orderNumber,
            String ref,
            String courier,
            String warehouse,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    ) throws BadRequestExceptions,InternalErrorExceptions;
}
