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
import java.util.concurrent.CompletableFuture;

public interface ICourier {
    CompletableFuture<ResponseSuccess> save(RequestCourier requestCourier, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String name,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> activate(String name,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<Page<CourierDTO>> list(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<Page<CourierDTO>> listFalse(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> updateOrder(Long orderId, RequestCourierOrder requestCourierOrder,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<List<CourierDTO>> listCouriers(String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<List<CourierDTO>> listCouriersFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
