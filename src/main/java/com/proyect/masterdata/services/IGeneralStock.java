package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IGeneralStock {
    public ResponseSuccess in(String supplierProductSerial, Integer quantity, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    public ResponseSuccess out(String supplierProductSerial, Integer quantity, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
}
