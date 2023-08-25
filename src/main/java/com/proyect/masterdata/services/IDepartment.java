package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IDepartment {
    ResponseSuccess save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> names, String user) throws BadRequestExceptions, InternalErrorExceptions;
    DepartmentDTO update(RequestDepartment requestDepartment) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions;

    List<DepartmentDTO> listDepartment();
    Page<DepartmentDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;

    Page<DepartmentDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    DepartmentDTO findByCode(Long code) throws BadRequestExceptions;

}
