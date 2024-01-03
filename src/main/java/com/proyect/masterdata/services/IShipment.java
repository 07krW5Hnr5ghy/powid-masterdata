package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.request.RequestShipment;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IShipment {
    ResponseSuccess save(String serial, String warehouse, List<RequestShipment> requestShipmentList, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
}