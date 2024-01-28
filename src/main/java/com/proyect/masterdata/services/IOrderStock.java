package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestOrderStock;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IOrderStock {
    public ResponseSuccess save(Long orderId, List<RequestOrderStock> requestOrderStocks,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
