package com.proyect.masterdata.services;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.dto.request.RequestModel;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ModelDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IModel {
        ResponseSuccess save(RequestModel requestModel)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(RequestModel requestModel)
                throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseDelete> delete(String sku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> activate(String sku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<ModelDTO>> list(
                String user,
                String name,
                List<String> brands,
                OffsetDateTime registrationStartDate,
                OffsetDateTime registrationEndDate,
                OffsetDateTime updateStartDate,
                OffsetDateTime updateEndDate,
                String sort,
                String columnSort,
                Integer pageNumber,
                Integer pageSize,
                Boolean status);
        CompletableFuture<List<ModelDTO>> listModels(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ModelDTO>> listModelsFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ModelDTO>> listModelBrand(String user,String brand) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ModelDTO>> listFilter(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
