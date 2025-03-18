package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CheckStockDTO;
import com.proyect.masterdata.dto.PurchaseOrderDTO;
import com.proyect.masterdata.dto.SupplyOrderDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseOrder;
import com.proyect.masterdata.dto.request.RequestSupplyOrder;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Repository
public interface IPurchaseOrder {
    ResponseSuccess save(RequestPurchaseOrder requestPurchaseOrder, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestPurchaseOrder requestPurchaseOrder, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<PurchaseOrderDTO>> list(
            Long orderNumber,
            String ref,
            String user,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseDelete> closePurchaseOrder(UUID purchaseOrderId, String username) throws BadRequestExceptions,InternalErrorExceptions;
}
