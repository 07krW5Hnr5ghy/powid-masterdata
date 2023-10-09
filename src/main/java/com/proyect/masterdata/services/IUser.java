package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.dto.request.RequestUser;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IUser {
    ResponseSuccess save(RequestUser requestUser) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseSuccess saveAll(List<RequestUser> requestUser, String user) throws BadRequestExceptions, InternalErrorExceptions;
    UserDTO update(RequestUser requestUser,String user) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseDelete delete(String user) throws BadRequestExceptions,InternalErrorExceptions;
    List<UserDTO> listUser() throws BadRequestExceptions;
    Page<UserDTO> list(String user,Long status, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<UserDTO> listStatusFalse(String user,Long status,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    UserDTO findByUser(String user) throws BadRequestExceptions;
}
