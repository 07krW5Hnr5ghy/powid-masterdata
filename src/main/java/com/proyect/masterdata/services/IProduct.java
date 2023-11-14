package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IProduct {

    ResponseSuccess save(RequestProductSave product, String user) throws InternalErrorExceptions, BadRequestExceptions;

    ResponseSuccess saveAll(List<RequestProductSave> products, String user)
            throws InternalErrorExceptions, BadRequestExceptions;
}
