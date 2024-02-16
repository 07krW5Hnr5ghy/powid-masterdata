package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestStockTransfer;
import com.proyect.masterdata.dto.request.RequestStockTransferItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IStockTransfer {
    public ResponseSuccess save(RequestStockTransfer requestStockTransfer, List<RequestStockTransferItem> requestStockTransferItems,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;

}
