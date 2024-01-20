package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.StateDTO;
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

    StateDTO update(RequestState requestState) throws BadRequestExceptions, InternalErrorExceptions;

    ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions;

    List<StateDTO> listState() throws BadRequestExceptions;

    Page<StateDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize)
            throws BadRequestExceptions;

    Page<StateDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;

    StateDTO findByCode(Long code) throws BadRequestExceptions;
}
