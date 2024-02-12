package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IOrderStock {
    public ResponseSuccess save(Long orderId, String warehouse, List<RequestOrderStockItem> requestOrderStockItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
}
