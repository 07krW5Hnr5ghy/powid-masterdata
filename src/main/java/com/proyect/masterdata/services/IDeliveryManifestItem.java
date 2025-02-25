package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.DeliveryManifest;
import com.proyect.masterdata.domain.DeliveryManifestItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
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
            RequestDeliveryManifestItem requestDeliveryManifestItem,
            DeliveryManifest deliveryManifest,
            Warehouse warehouse,
            User user) throws BadRequestExceptions,InterruptedException;
    CompletableFuture<ResponseSuccess> updateDeliveryManifestItem(
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
            String supplier,
            String brand,
            String deliveryStatus,
            String courier,
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
}
