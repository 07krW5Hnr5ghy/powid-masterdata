package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.request.RequestSupplier;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISupplier {
    ResponseSuccess save(RequestSupplier requestSupplier, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    ResponseSuccess saveAll(List<RequestSupplier> requestSuppliers, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    ResponseDelete delete(String ruc, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
