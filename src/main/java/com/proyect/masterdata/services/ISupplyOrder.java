package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CheckStockDTO;
import com.proyect.masterdata.dto.SupplyOrderDTO;
import com.proyect.masterdata.dto.request.RequestSupplyOrder;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface ISupplyOrder {
    ResponseSuccess save(RequestSupplyOrder requestSupplyOrder, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestSupplyOrder requestSupplyOrder, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<SupplyOrderDTO>> list(
            String ref,
            String user,
            String warehouse,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<SupplyOrderDTO>> listFalse(
            String ref,
            String user,
            String warehouse,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<SupplyOrderDTO>> listPurchase(String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<SupplyOrderDTO>> listFilter(String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<CheckStockDTO>> checkStock(UUID productId, String user) throws BadRequestExceptions,InternalErrorExceptions;
}
