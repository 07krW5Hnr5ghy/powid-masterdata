package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestCustomer;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ICustomer {
    public ResponseSuccess save(RequestCustomer requestCustomer, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;

}
