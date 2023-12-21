package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SupplierDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISupplier {
    ResponseSuccess save(SupplierDTO supplierData, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
}
