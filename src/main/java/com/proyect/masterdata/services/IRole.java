package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.RoleDTO;
import com.proyect.masterdata.dto.request.RequestRole;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IRole {
        ResponseSuccess save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;

        ResponseSuccess saveAll(List<String> names, String user) throws BadRequestExceptions, InternalErrorExceptions;

        RoleDTO update(RequestRole requestUserRole) throws BadRequestExceptions, InternalErrorExceptions;

        ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions;

        Page<RoleDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize)
                        throws BadRequestExceptions;

        Page<RoleDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize) throws BadRequestExceptions;

        ResponseSuccess addAccess(String role, String access, String user)
                        throws BadRequestExceptions, InternalErrorExceptions;

}
