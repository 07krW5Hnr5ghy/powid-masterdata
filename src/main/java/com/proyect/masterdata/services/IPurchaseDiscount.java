package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PurchaseDiscountDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseDiscount;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IPurchaseDiscount {
    ResponseSuccess save(RequestPurchaseDiscount requestPurchaseDiscount) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestPurchaseDiscount requestPurchaseDiscount) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<PurchaseDiscountDTO>> listPurchaseDiscount() throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<PurchaseDiscountDTO>> list(
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
