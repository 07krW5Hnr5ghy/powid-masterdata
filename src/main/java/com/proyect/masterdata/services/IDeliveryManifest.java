package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestDeliveryManifest;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.concurrent.CompletableFuture;

public interface IDeliveryManifest {
    CompletableFuture<ResponseSuccess> save(RequestDeliveryManifest requestDeliveryManifest) throws InternalErrorExceptions, BadRequestExceptions;
}
