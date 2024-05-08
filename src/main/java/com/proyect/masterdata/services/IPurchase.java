package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchase;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IPurchase {
    ResponseSuccess save(RequestPurchase requestPurchase) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestPurchase requestPurchase) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<PurchaseDTO>> list(String serial,String user,String documentName,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<PurchaseDTO>> listPurchase(String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<PurchaseDTO>> listPurchaseFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
