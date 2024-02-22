package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderPaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestOrderPaymentState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IOrderPaymentState {
    ResponseSuccess save(String name,String user) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions,InternalErrorExceptions;
    OrderPaymentStateDTO update(RequestOrderPaymentState requestOrderPaymentState) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<OrderPaymentStateDTO> listPaymentState() throws BadRequestExceptions;
    Page<OrderPaymentStateDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<OrderPaymentStateDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    OrderPaymentStateDTO findByCode(Long code) throws BadRequestExceptions;
}
