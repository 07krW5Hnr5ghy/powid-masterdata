package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.request.RequestOrderUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IOrdering {
    ResponseSuccess save(RequestOrderSave requestOrderSave, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestOrderSave requestOrderSave, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<OrderDTO>> list(Long orderId,String user,String orderState,String courier,String paymentState,String paymentMethod,String saleChannel,String managementType,String storeName,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<OrderDTO>> listOrder(String user) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseSuccess update(Long orderId, RequestOrderUpdate requestOrderUpdate,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> updateAsync(Long orderId, RequestOrderUpdate requestOrderUpdate,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
