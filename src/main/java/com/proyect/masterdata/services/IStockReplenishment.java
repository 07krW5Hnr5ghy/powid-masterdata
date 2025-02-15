package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.StockReplenishmentItem;
import com.proyect.masterdata.dto.StockReplenishmentDTO;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IStockReplenishment {
    ResponseSuccess save(UUID orderId, List<RequestStockReplenishmentItem> requestStockReplenishmentItems, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(UUID orderId, List<RequestStockReplenishmentItem> requestStockReplenishmentItems, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<StockReplenishmentDTO>> list(
            String user,
            List<UUID> orderIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<StockReplenishmentDTO>> listStockReplenishment(String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<StockReplenishmentDTO>> listStockReplenishmentFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
