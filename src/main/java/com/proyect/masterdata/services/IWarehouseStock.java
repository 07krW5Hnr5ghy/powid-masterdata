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

public interface IWarehouseStock {
        ResponseSuccess in(Warehouse warehouse, SupplierProduct supplierProduct, Integer quantity, User user)
                        throws InternalErrorExceptions, BadRequestExceptions;
        ResponseSuccess out(Warehouse warehouse, SupplierProduct supplierProduct, Integer quantity, User user)
                        throws InternalErrorExceptions, BadRequestExceptions;
        Page<WarehouseStockDTO> list(String warehouse, String user, String sort, String sortColumn,
                        Integer pageNumber,
                        Integer pageSize) throws InternalErrorExceptions;
        List<WarehouseStockDTO> listWarehouse(String user,Long warehouseId) throws BadRequestExceptions,InternalErrorExceptions;

}
