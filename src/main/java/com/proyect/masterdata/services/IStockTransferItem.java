package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.StockTransfer;
import com.proyect.masterdata.domain.StockTransferItem;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.StockTransferItemDTO;
import com.proyect.masterdata.dto.request.RequestStockTransferItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IStockTransferItem {
    StockTransferItem save(RequestStockTransferItem requestStockTransferItem, StockTransfer stockTransfer, SupplierProduct supplierProduct, User user) throws InternalErrorExceptions, BadRequestExceptions;
    Page<StockTransferItemDTO> list(String user,Long stockTransferId,String supplierProductSerial,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    List<StockTransferItemDTO> listStockTransferItem(String user,Long id) throws BadRequestExceptions,InternalErrorExceptions;
}
