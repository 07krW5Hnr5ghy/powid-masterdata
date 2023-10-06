package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.dto.request.RequestUser;
import com.proyect.masterdata.dto.request.RequestUserSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IUser {
    ResponseSuccess save(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseSuccess saveAll(List<RequestUserSave> requestUserSaveList, String user) throws BadRequestExceptions, InternalErrorExceptions;
    UserDTO update(RequestUser requestUser) throws BadRequestExceptions,InternalErrorExceptions;
}
