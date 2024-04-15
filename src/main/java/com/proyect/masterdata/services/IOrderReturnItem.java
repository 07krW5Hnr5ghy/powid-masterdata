package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IOrderReturnItem {
    ResponseSuccess save(Long orderReturnId,Long orderId,RequestOrderReturnItem requestOrderReturnItem,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    List<OrderReturnItemDTO> list(String user,Long orderId) throws BadRequestExceptions;
}
