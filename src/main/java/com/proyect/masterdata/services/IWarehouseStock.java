package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.WarehouseStockDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IWarehouseStock {
        ResponseSuccess in(Warehouse warehouse, SupplierProduct supplierProduct, Integer quantity, User user)
                        throws InternalErrorExceptions, BadRequestExceptions;
        ResponseSuccess out(Warehouse warehouse, SupplierProduct supplierProduct, Integer quantity, User user)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<WarehouseStockDTO>> list(
                List<String> warehouses,
                String serial,
                String productSku,
                String model,
                String user,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize) throws InternalErrorExceptions;
        CompletableFuture<List<WarehouseStockDTO>> listWarehouse(String user, String warehouse, UUID supplierProductId) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<WarehouseStockDTO>> listWarehouseAndSupplier(String user,String warehouse,String supplier) throws BadRequestExceptions,InternalErrorExceptions;

}
