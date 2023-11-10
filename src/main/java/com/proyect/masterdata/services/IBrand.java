package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IBrand {

    ResponseSuccess save(String name, String user) throws InternalErrorExceptions, BadRequestExceptions;

    ResponseSuccess saveAll(List<String> namesList, String user) throws InternalErrorExceptions, BadRequestExceptions;

    ResponseDelete delete(String name, String user) throws InternalErrorExceptions, BadRequestExceptions;
}
