package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.OrderContacted;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IOrderContacted {
    CompletableFuture<OrderContacted> save(UUID orderId, String username) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> markContacted(UUID orderId, String username) throws BadRequestExceptions,InternalErrorExceptions;
}
