package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IUserRole {
    ResponseSuccess save(String username, String role, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
}
