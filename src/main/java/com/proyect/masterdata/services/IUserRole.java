package com.proyect.masterdata.services;


import com.proyect.masterdata.dto.UserRoleDTO;
import com.proyect.masterdata.dto.request.RequestUserRole;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IUserRole {
    ResponseSuccess save(String name,String user) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions,InternalErrorExceptions;
    UserRoleDTO update(RequestUserRole requestUserRole) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<UserRoleDTO> listUserRole() throws BadRequestExceptions;
    List<UserRoleDTO> listStatusFalse() throws BadRequestExceptions;
    UserRoleDTO findByCode(Long code) throws BadRequestExceptions;
}
