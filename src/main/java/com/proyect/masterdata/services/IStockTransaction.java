package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.StockTransaction;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IStockTransaction {
    public StockTransaction save(String serial, String warehouse, List<RequestStockTransactionItem> requestStockTransactionItemList, String stockTransactionType, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
}
