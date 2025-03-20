package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderPaymentMethodDTO;
import com.proyect.masterdata.dto.PurchasePaymentMethodDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IPurchasePaymentMethod {
    CompletableFuture<ResponseSuccess> save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(String name,String user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<PurchasePaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions;
    CompletableFuture<Page<PurchasePaymentMethodDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize,Boolean status) throws BadRequestExceptions;
    CompletableFuture<List<PurchasePaymentMethodDTO>> listFilter() throws BadRequestExceptions;
}
