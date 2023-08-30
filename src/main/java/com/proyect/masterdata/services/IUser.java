package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestUserSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IUser {
    ResponseSuccess save(RequestUserSave requestUserSave) throws BadRequestExceptions, InternalErrorExceptions;
}
