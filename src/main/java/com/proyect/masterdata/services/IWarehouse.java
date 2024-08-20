package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.dto.response.ResponseDelete;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.WarehouseDTO;
import com.proyect.masterdata.dto.request.RequestWarehouse;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IWarehouse {
        ResponseSuccess save(RequestWarehouse requestWarehouse, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(RequestWarehouse requestWarehouse, String tokenUser)
                throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseDelete> delete(String warehouse,String tokenUser) throws BadRequestExceptions;
        CompletableFuture<ResponseSuccess> activate(String warehouse,String tokenUser) throws BadRequestExceptions;
        CompletableFuture<Page<WarehouseDTO>> list(
                String user,
                List<String> names,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<List<WarehouseDTO>> listWarehouse(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<WarehouseDTO>> listWarehouseFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<WarehouseDTO>> listFilters(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
