package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.StateDTO;
import com.proyect.masterdata.dto.request.RequestState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IState {
    ResponseSuccess save(String name,String user) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions;
    StateDTO update(RequestState requestState) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<StateDTO> list() throws BadRequestExceptions;
    StateDTO findByCode(Long code) throws BadRequestExceptions;
    StateDTO findByName(String name) throws BadRequestExceptions;
}
