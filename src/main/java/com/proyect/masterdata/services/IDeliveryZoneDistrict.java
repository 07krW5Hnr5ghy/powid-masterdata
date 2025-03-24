package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.concurrent.CompletableFuture;

public interface IDeliveryZoneDistrict {
    ResponseSuccess save(String deliveryZoneName, String districtName, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(String deliveryZoneName, String districtName, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String deliveryZoneName, String districtName, String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> activate(String deliveryZoneName, String districtName, String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
