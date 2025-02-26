package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DeliveryCompanyDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IDeliveryCompany {
    CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<DeliveryCompanyDTO>> listDeliveryCompany() throws BadRequestExceptions;
    CompletableFuture<Page<DeliveryCompanyDTO>> list(
            String user,
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<Page<DeliveryCompanyDTO>> listFalse(
            String user,
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
}
