package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IOrderReturn {
    ResponseSuccess save(Long orderId, List<RequestOrderReturnItem> requestOrderReturnItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
}
