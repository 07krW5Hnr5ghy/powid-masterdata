package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IOrderReturnItem {
    CompletableFuture<ResponseSuccess> save(Long orderReturnId, Long orderId, RequestOrderReturnItem requestOrderReturnItem, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<OrderReturnItemDTO>> list(String user,Long orderId) throws BadRequestExceptions;
}
