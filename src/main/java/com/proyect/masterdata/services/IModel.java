package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IModel {

    ResponseSuccess save(String name, String brand, String user) throws InternalErrorExceptions, BadRequestExceptions;

    ResponseSuccess saveAll(List<String> names, String brand, String user)
            throws InternalErrorExceptions, BadRequestExceptions;
}
