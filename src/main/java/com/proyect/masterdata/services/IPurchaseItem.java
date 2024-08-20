package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.PurchaseItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.PurchaseItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IPurchaseItem {
    PurchaseItem save(Purchase purchase, String warehouse, RequestPurchaseItem requestPurchaseItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<PurchaseItem> saveAsync(Purchase purchase, String warehouse, RequestPurchaseItem requestPurchaseItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String serial,String supplierProduct, String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(String serial,String supplierProduct, String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<PurchaseItemDTO>> list(
            String user,
            List<String> purchases,
            List<String> warehouses,
            List<String> supplierProducts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<PurchaseItemDTO>> listPurchaseItem(String user, Long id) throws InternalErrorExceptions,BadRequestExceptions;
}