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
        ResponseDelete delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;
        Page<RoleDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize)
                        throws BadRequestExceptions;
        Page<RoleDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize) throws BadRequestExceptions;
        ResponseSuccess activate(String name,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
}
