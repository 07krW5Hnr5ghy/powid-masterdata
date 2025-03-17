package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SupplierDTO;
import com.proyect.masterdata.dto.request.RequestSupplier;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ISupplier {
    ResponseSuccess save(RequestSupplier requestSupplier, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestSupplier requestSupplier, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String ruc, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> activate(String ruc, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<SupplierDTO>> list(
            String user,
            List<String> names,
            List<String> rucs,
            List<String> countries,
            List<String> supplierTypes,
            List<String> departments,
            List<String> provinces,
            List<String> districts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) throws BadRequestExceptions;
    CompletableFuture<List<SupplierDTO>> listSuppliers(String user) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<List<SupplierDTO>> listSuppliersFalse(String user) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<List<SupplierDTO>> listSuppliersFilter(String user) throws InternalErrorExceptions,BadRequestExceptions;
}
