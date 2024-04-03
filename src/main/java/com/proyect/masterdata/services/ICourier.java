package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CourierDTO;
import com.proyect.masterdata.dto.request.RequestCourier;
import com.proyect.masterdata.dto.request.RequestCourierOrder;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface ICourier {
    ResponseSuccess save(RequestCourier requestCourier,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseDelete delete(String name,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    Page<CourierDTO> list(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    Page<CourierDTO> listFalse(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    ResponseSuccess updateOrder(Long orderId, RequestCourierOrder requestCourierOrder,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    List<CourierDTO> listCouriers(String user) throws BadRequestExceptions,InternalErrorExceptions;
    List<CourierDTO> listCouriersFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
