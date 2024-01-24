package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderStateDTO;
import com.proyect.masterdata.dto.request.RequestState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IOrderState {
    ResponseSuccess save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;

    ResponseSuccess saveAll(List<String> names, String user) throws BadRequestExceptions, InternalErrorExceptions;

    OrderStateDTO update(RequestState requestState) throws BadRequestExceptions, InternalErrorExceptions;

    ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions;

    List<OrderStateDTO> listState() throws BadRequestExceptions;

    Page<OrderStateDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize)
            throws BadRequestExceptions;

    Page<OrderStateDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
                                        Integer pageSize) throws BadRequestExceptions;

    OrderStateDTO findByCode(Long code) throws BadRequestExceptions;
}
