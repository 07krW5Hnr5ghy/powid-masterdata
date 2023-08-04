package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IDepartment {
    ResponseSuccess save(String name, String user) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names, String user) throws BadRequestExceptions;
    DepartmentDTO update(RequestDepartment requestDepartment) throws BadRequestExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes, String user) throws BadRequestExceptions;
    List<DepartmentDTO> list() throws BadRequestExceptions;

    List<DepartmentDTO> listStatusFalse() throws BadRequestExceptions;
    DepartmentDTO findByCode(Long code) throws BadRequestExceptions;
    DepartmentDTO findByName(String name) throws BadRequestExceptions;
    DepartmentDTO findByUser(String user) throws BadRequestExceptions;
}
