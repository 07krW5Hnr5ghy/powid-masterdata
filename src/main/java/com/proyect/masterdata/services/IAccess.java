package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.AccessDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IAccess {

    ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseDelete delete(String name,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    List<AccessDTO> list() throws BadRequestExceptions;
    List<AccessDTO> listFalse() throws BadRequestExceptions;
}
