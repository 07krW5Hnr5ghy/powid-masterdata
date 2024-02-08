package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CourierDTO;
import com.proyect.masterdata.dto.request.RequestCourier;
import com.proyect.masterdata.dto.request.RequestCourierOrder;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

public interface ICourier {
    public ResponseSuccess save(RequestCourier requestCourier,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    public ResponseDelete delete(String name,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    public Page<CourierDTO> list(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    public Page<CourierDTO> listFalse(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    public ResponseSuccess updateOrder(Long orderId, RequestCourierOrder requestCourierOrder,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
