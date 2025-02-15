package com.proyect.masterdata.services;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.dto.response.ResponseDelete;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.CategoryProductDTO;
import com.proyect.masterdata.dto.request.RequestCategoryProduct;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ICategoryProduct {
    ResponseSuccess save(String name, String description,String sizeTypeName, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(String name, String description, String sizeTypeName, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<CategoryProductDTO>> list(
            String name,
            String user,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<Page<CategoryProductDTO>> listFalse(
            String name,
            String user,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<CategoryProductDTO>> listCategoryProducts() throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String name,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(String name,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> update(String name,String description,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<CategoryProductDTO>> listFilter() throws InternalErrorExceptions,BadRequestExceptions;
}
