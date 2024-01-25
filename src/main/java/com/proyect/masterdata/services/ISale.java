package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Order;
import com.proyect.masterdata.dto.request.RequestSale;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISale {
    ResponseSuccess save(Order order,RequestSale requestSale, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
