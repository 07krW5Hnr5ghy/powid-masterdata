package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IModuleType {
    ResponseSuccess save(String userType,String module,String user) throws InternalErrorExceptions, BadRequestExceptions;
}
