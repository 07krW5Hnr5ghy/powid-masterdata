package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface ICancellationReason {
    ResponseSuccess save(String name,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    List<String> list() throws BadRequestExceptions;
    List<String> listFalse() throws BadRequestExceptions;
    ResponseDelete delete(String name,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    ResponseSuccess activate(String name,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
