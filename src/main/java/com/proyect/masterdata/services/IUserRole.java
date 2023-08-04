package com.proyect.masterdata.services;


import com.proyect.masterdata.dto.UserRoleDTO;
import com.proyect.masterdata.dto.request.RequestUserRole;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IUserRole {
    ResponseSuccess save(String name) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions;
    UserRoleDTO update(RequestUserRole requestUserRole) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<UserRoleDTO> list() throws BadRequestExceptions;
    UserRoleDTO findByCode(Long code) throws BadRequestExceptions;
    UserRoleDTO findByName(String name) throws BadRequestExceptions;
}
