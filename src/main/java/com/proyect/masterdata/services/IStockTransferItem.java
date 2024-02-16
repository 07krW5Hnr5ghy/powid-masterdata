package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.StockTransfer;
import com.proyect.masterdata.domain.StockTransferItem;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestStockTransferItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IStockTransferItem {
    public StockTransferItem save(RequestStockTransferItem requestStockTransferItem, StockTransfer stockTransfer, SupplierProduct supplierProduct, User user) throws InternalErrorExceptions, BadRequestExceptions;
}
