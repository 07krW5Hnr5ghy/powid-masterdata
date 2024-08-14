package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CheckStockDTO;
import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchase;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IPurchase {
    ResponseSuccess save(RequestPurchase requestPurchase, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestPurchase requestPurchase, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<PurchaseDTO>> list(
            List<String> serials,
            String user,
            List<String> warehouses,
            List<String> purchaseTypes,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<PurchaseDTO>> listFalse(
            List<String> serials,
            String user,
            List<String> warehouses,
            List<String> purchaseTypes,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<PurchaseDTO>> listPurchase(String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<PurchaseDTO>> listFilter(String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<CheckStockDTO>> checkStock(String serial, String user) throws BadRequestExceptions,InternalErrorExceptions;
}
