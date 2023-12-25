package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.dto.UserQueryDTO;
import com.proyect.masterdata.dto.request.RequestUser;
import com.proyect.masterdata.dto.request.RequestUserSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

public interface IUser {
    ResponseSuccess save(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions;

    UserDTO update(RequestUserSave requestUserSave, String user) throws BadRequestExceptions, InternalErrorExceptions;

    ResponseDelete delete(String user) throws BadRequestExceptions, InternalErrorExceptions;

    Page<UserQueryDTO> list(String user, String clientRuc, String sort, String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
}
