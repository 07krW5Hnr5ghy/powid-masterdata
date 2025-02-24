package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.DeliveryManifest;
import com.proyect.masterdata.domain.DeliveryManifestItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.request.RequestDeliveryManifestItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

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
}
