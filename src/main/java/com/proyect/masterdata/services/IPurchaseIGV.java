package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PurchaseIGVDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseIGV;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IPurchaseIGV {
    ResponseSuccess save(RequestPurchaseIGV requestPurchaseIGV) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestPurchaseIGV requestPurchaseIGV) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<PurchaseIGVDTO>> listPurchaseDiscount() throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<PurchaseIGVDTO>> list(
            String user,
            String name,
            Double value,
            Boolean percentage,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    ) throws BadRequestExceptions,InternalErrorExceptions;
}
