package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISubscription {
    ResponseSuccess save(String name, Integer months, Double discountPercent)
            throws InternalErrorExceptions, BadRequestExceptions;
}
