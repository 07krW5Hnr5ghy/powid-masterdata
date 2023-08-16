package com.proyect.masterdata.services;


import com.proyect.masterdata.dto.UserRoleDTO;
import com.proyect.masterdata.dto.request.RequestUserRole;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IUserRole {
    ResponseSuccess save(String name,String user) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions;
    UserRoleDTO update(RequestUserRole requestUserRole) throws BadRequestExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes,String user) throws BadRequestExceptions;
    List<UserRoleDTO> list() throws BadRequestExceptions;
    List<UserRoleDTO> listStatusFalse() throws BadRequestExceptions;
    UserRoleDTO findByCode(Long code) throws BadRequestExceptions;
    UserRoleDTO findByName(String name) throws BadRequestExceptions;
    List<UserRoleDTO> findByUser(String user) throws BadRequestExceptions;
}
