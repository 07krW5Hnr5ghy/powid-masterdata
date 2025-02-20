package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IProductPrice {
    ResponseSuccess save(UUID productId, Double unitPrice, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(UUID productId, Double unitPrice, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(UUID productId,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
