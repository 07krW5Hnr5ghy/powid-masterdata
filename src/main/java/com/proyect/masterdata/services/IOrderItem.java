package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.dto.request.RequestOrderItem;
import com.proyect.masterdata.dto.response.ResponseCheckStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IOrderItem {
    public ResponseSuccess save(Ordering ordering, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    public ResponseCheckStockItem checkStock(String productSku,Integer quantity,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    public ResponseDelete delete(Long orderId,Long itemId,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
