package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.dto.OrderItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderItem;
import com.proyect.masterdata.dto.response.ResponseCheckStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IOrderItem {
    ResponseSuccess save(Ordering ordering, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseCheckStockItem checkStock(String productSku,Integer quantity,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    ResponseDelete delete(Long orderId,String productSku,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    ResponseSuccess add(Long orderId,RequestOrderItem requestOrderItem,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    ResponseSuccess update(Long orderId,RequestOrderItem requestOrderItem,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    List<OrderItemDTO> listOrderItems(String user,Long id) throws BadRequestExceptions,InternalErrorExceptions;
}
