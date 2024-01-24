package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestOrder;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IOrder {
    ResponseSuccess save(RequestOrder requestOrder,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
