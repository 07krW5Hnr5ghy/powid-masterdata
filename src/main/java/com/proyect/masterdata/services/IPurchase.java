package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.request.RequestPurchase;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IPurchase {

    public ResponseSuccess save(String serial, List<RequestPurchase> items, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
}
