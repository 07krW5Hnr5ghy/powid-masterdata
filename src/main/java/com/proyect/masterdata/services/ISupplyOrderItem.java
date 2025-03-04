package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.SupplyOrder;
import com.proyect.masterdata.domain.SupplyOrderItem;
import com.proyect.masterdata.dto.SupplyOrderItemDTO;
import com.proyect.masterdata.dto.request.RequestSupplyOrderItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface ISupplyOrderItem {
    SupplyOrderItem save(SupplyOrder supplyOrder, String warehouse, RequestSupplyOrderItem requestSupplyOrderItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(UUID purchaseId, RequestSupplyOrderItem requestSupplyOrderItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(UUID purchaseId,UUID productId, String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(UUID purchaseId,UUID productId, String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<SupplyOrderItemDTO>> list(
            String user,
            Long purchaseNumber,
            String warehouse,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<SupplyOrderItemDTO>> listPurchaseItem(String user, UUID id) throws InternalErrorExceptions,BadRequestExceptions;
}