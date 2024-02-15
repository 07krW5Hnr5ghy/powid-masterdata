package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.StockReplenishmentItem;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IStockReplenishment {
    public ResponseSuccess save(Long orderId, List<RequestStockReplenishmentItem> requestStockReplenishmentItems, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
