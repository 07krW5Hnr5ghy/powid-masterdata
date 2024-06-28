package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.Model;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ProductDTO;
import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IProduct {
        ResponseSuccess save(RequestProductSave product, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(RequestProductSave product, String tokenUser)
                throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseDelete> delete(String sku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> activate(String sku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<ProductDTO>> list(String sku, String model, String tokenUser, String sort, String sortColumn, Integer pageNumber,
                              Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<Page<ProductDTO>> listFalse(String sku, String model, String tokenUser, String sort, String sortColumn, Integer pageNumber,
                              Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<List<ProductDTO>> listProducts(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listProductsFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listProductsModel(String user,String model) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listFilter(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
