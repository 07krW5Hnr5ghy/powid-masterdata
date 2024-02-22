package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderPaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestOrderPaymentMethod;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IOrderPaymentMethod {
    ResponseSuccess save(String name, String user) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions,InternalErrorExceptions;
    OrderPaymentMethodDTO update(RequestOrderPaymentMethod requestOrderPaymentMethod) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<OrderPaymentMethodDTO> listPaymentMethod() throws BadRequestExceptions;
    Page<OrderPaymentMethodDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<OrderPaymentMethodDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    OrderPaymentMethodDTO findByCode(Long code) throws BadRequestExceptions;
}
