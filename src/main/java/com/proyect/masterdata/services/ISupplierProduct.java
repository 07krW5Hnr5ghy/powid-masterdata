package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.SupplierProductDTO;
import com.proyect.masterdata.dto.request.RequestSupplierProduct;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISupplierProduct {
        ResponseSuccess save(RequestSupplierProduct requestSupplierProduct, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(RequestSupplierProduct requestSupplierProduct, String tokenUser)
                throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseDelete> delete(String serial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> activate(String serial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<SupplierProductDTO>> list(
                String user,
                List<String> serial,
                List<String> products,
                List<String> suppliers,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<Page<SupplierProductDTO>> listFalse(
                String user,
                List<String> serial,
                List<String> products,
                List<String> suppliers,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<List<SupplierProductDTO>> listSupplierProduct(String user, String supplier) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<SupplierProductDTO>> listFilter(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<SupplierProductDTO>> listSupplierProductFalse(String user,Long id) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<SupplierProductDTO>> listSupplierProductByProduct(String user,String productSku) throws BadRequestExceptions,InternalErrorExceptions;

}
