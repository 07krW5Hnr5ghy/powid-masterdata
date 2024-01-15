package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IWarehouseStock {
    public ResponseSuccess in(String warehouse, String supplierProductSerial, Integer quantity, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    public ResponseSuccess out(String warehouse, String supplierProductSerial, Integer quantity, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
}
