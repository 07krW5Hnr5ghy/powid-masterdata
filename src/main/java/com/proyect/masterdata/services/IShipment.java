package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IShipment {
    public ResponseSuccess save(String serial, String warehouse, List<RequestShipmentItem> requestShipmentItemList,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
}
