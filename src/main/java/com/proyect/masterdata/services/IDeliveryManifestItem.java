package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CourierProfileDTO;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.dto.request.RequestDeliveryManifestItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IDeliveryManifestItem {
    CompletableFuture<DeliveryManifestItem> save(
            OrderItem orderItem,
            DeliveryManifest deliveryManifest,
            Warehouse warehouse,
            User user) throws BadRequestExceptions,InterruptedException;
    CompletableFuture<ResponseSuccess> markDeliveredDeliveryManifestItem(
            UUID deliveryManifestItemId,
            String user
    );
    CompletableFuture<ResponseSuccess> markCollectedDeliveryManifestItem(
            UUID deliveryManifestItemId,
            String user
    );
    CompletableFuture<Page<DeliveryManifestItemDTO>> list(
            String user,
            Integer quantity,
            Boolean collected,
            Long orderNumber,
            Long manifestNumber,
            String color,
            String size,
            String model,
            String brand,
            Boolean delivered,
            String courier,
            String courierDni,
            String warehouse,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
    CompletableFuture<CourierProfileDTO> courierProfile(
            OffsetDateTime startDate,
            OffsetDateTime endDate,
            String username
    );
}
