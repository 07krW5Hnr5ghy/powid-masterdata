package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestDeliveryManifestOrder;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IDeliveryManifestOrder {
    CompletableFuture<ResponseSuccess> save(RequestDeliveryManifestOrder requestDeliveryManifestOrder) throws BadRequestExceptions, InternalErrorExceptions;
}
