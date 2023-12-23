package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.request.RequestSupplierProduct;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISupplierProduct {

    ResponseSuccess save(RequestSupplierProduct requestSupplierProduct, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    ResponseSuccess saveAll(List<RequestSupplierProduct> requestSupplierProducts, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    ResponseDelete delete(String serial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
