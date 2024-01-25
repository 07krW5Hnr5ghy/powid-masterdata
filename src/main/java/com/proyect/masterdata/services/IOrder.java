package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IOrder {
    ResponseSuccess save(RequestOrderSave requestOrderSave, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
