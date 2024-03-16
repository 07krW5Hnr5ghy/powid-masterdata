package com.proyect.masterdata.services;

import java.util.List;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.SupplierDTO;
import com.proyect.masterdata.dto.request.RequestSupplier;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISupplier {
        ResponseSuccess save(RequestSupplier requestSupplier, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        ResponseDelete delete(String ruc, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        Page<SupplierDTO> list(String name, String ruc, String user, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize) throws BadRequestExceptions;
}
