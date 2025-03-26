package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DeliveryManifestCourierDTO;
import com.proyect.masterdata.dto.DeliveryManifestDTO;
import com.proyect.masterdata.dto.request.RequestDeliveryManifest;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IDeliveryManifest {
    CompletableFuture<ResponseSuccess> save(RequestDeliveryManifest requestDeliveryManifest) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<DeliveryManifestDTO> getById(UUID deliveryManifestId,String user) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> closeDeliveryManifest(UUID deliveryManifestId,String user) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<Page<DeliveryManifestDTO>> list(
            String user,
            Long manifestNumber,
            String warehouse,
            String courier,
            String courierDni,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean open
    ) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<DeliveryManifestCourierDTO> checkCourierToDeliveryManifest(UUID courierId) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<DeliveryManifestDTO> getLastDeliveryManifestByCourier(String username) throws BadRequestExceptions,InternalErrorExceptions;
}
