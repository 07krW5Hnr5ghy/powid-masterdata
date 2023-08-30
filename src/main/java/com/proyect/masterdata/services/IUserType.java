package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.UserTypeDTO;
import com.proyect.masterdata.dto.request.RequestUserType;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;
import java.util.List;

public interface IUserType {
    ResponseSuccess save(String usertype, String user) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> usertype, String user) throws BadRequestExceptions, InternalErrorExceptions;
    UserTypeDTO update(RequestUserType requestUserType) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<UserTypeDTO> listUserType();
    Page<UserTypeDTO> list(String usertype, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<UserTypeDTO> listStatusFalse(String usertype, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    UserTypeDTO findByCode(Long code) throws BadRequestExceptions;
}
