package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestWarehouseOutput;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IWarehouseOutput {
    CompletableFuture<ResponseSuccess> save(RequestWarehouseOutput requestWarehouseOutput) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseDelete> close(String username, UUID warehouseOutputId) throws BadRequestExceptions,InternalErrorExceptions;
}
