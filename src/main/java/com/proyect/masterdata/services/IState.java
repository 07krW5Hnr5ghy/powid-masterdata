package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.StateDTO;
import com.proyect.masterdata.dto.response.ResponseState;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IState {
    //List<StateDTO> listState() throws BadRequestExceptions;
    ResponseState addState(String state) throws BadRequestExceptions;
    ResponseState deleteState(Long id) throws BadRequestExceptions;
    StateDTO updateState(String name,Long id) throws BadRequestExceptions;
}
