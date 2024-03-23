package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReturnDTO;
import com.proyect.masterdata.dto.StockReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IStockReturnItem {
    public StockReturnItem save(StockReturn stockReturn, PurchaseItem purchaseItem, RequestStockReturnItem requestStockReturnItem, User user) throws InternalErrorExceptions, BadRequestExceptions;
    Page<StockReturnItemDTO> list(String purchaseSerial, String user, String supplierProductSerial, String sort, String sortColumn,
                                  Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    List<StockReturnItemDTO> listStockReturnItem(String user) throws InternalErrorExceptions,BadRequestExceptions;
    List<StockReturnItemDTO> listStockReturnItemFalse(String user) throws InternalErrorExceptions,BadRequestExceptions;
}
