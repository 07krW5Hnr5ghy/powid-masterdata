package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.dto.response.ResponseDelete;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.PurchaseItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IPurchaseItem {
    ResponseSuccess save(Long purchaseId, RequestPurchaseItem requestPurchaseItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(Long purchaseId, RequestPurchaseItem requestPurchaseItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<PurchaseItemDTO>> list(
            List<String> serials,
            String user,
            List<String> suppliers,
            List<String> supplierProducts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String purchaseSerial,String serialSupplierProduct,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> activate(String purchaseSerial,String serialSupplierProduct,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<List<PurchaseItemDTO>> listPurchaseItem(String user,Long id) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<PurchaseItemDTO>> listPurchaseItemFalse(String user,Long id) throws BadRequestExceptions,InternalErrorExceptions;

}
