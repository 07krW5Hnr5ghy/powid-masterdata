package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.PurchaseOrder;
import com.proyect.masterdata.domain.PurchaseOrderItem;
import com.proyect.masterdata.dto.PurchaseOrderItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseOrderItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IPurchaseOrderItem {
    PurchaseOrderItem save(PurchaseOrder purchaseOrder, RequestPurchaseOrderItem requestPurchaseOrderItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(UUID purchaseId, RequestPurchaseOrderItem requestPurchaseOrderItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(UUID purchaseId, UUID productId, String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(UUID purchaseId,UUID productId, String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<PurchaseOrderItemDTO>> list(
            String user,
            Long orderNumber,
            String ref,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<PurchaseOrderItemDTO>> listPurchaseOrderItem(String user, UUID id) throws InternalErrorExceptions,BadRequestExceptions;
}
