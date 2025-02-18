package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestSubCategoryProduct;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.concurrent.CompletableFuture;

public interface ISubCategoryProduct {
    ResponseSuccess save(RequestSubCategoryProduct requestSubCategoryProduct) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestSubCategoryProduct requestSubCategoryProduct) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(String name,String sku,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(String name,String sku,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;

}
