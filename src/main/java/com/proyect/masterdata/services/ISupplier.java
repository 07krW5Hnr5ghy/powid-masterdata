package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.SupplierDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISupplier {
    ResponseSuccess save(SupplierDTO supplierData, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    ResponseSuccess saveAll(List<SupplierDTO> supplierDataList, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    ResponseDelete delete(String ruc, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
