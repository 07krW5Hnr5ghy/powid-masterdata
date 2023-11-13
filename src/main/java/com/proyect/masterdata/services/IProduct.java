package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IProduct {
    ResponseSuccess save(String sku, String model, String size, String category, String color, String user)
            throws InternalErrorExceptions, BadRequestExceptions;
}
