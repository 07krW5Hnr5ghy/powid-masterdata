package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.CategoryProductDTO;
import com.proyect.masterdata.dto.SubCategoryProductDTO;
import com.proyect.masterdata.dto.request.RequestSubCategoryProduct;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ISubCategoryProduct {
    ResponseSuccess save(RequestSubCategoryProduct requestSubCategoryProduct) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestSubCategoryProduct requestSubCategoryProduct) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(String name,String sku,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(String name,String sku,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<SubCategoryProductDTO>> list(
            String name,
            String user,
            String sku,
            List<String> categoryProducts,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;

    CompletableFuture<Page<SubCategoryProductDTO>> listFalse(
            String name,
            String user,
            String sku,
            List<String> categoryProducts,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;

}
