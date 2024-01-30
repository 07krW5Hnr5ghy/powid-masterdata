package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderStockDTO;
import com.proyect.masterdata.dto.request.RequestOrderStock;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IOrderStock {
    public ResponseSuccess save(Long orderId, List<RequestOrderStock> requestOrderStocks,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    public Page<OrderStockDTO> list(String warehouse,Long orderId,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;

    public Page<OrderStockDTO> listFalse(String warehouse,Long orderId,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;

}
