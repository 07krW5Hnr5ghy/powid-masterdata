package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IRoleAccess {
    ResponseSuccess save(String role, String Access, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
}
