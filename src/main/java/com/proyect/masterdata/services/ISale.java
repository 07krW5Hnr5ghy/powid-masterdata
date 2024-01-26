package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.dto.request.RequestSale;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISale {
    ResponseSuccess save(Ordering ordering, RequestSale requestSale, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
