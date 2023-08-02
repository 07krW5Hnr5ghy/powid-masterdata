package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IPaymentMethod {
    ResponseSuccess save(String name) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions;
    DepartmentDTO update(RequestDepartment requestDepartment) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<DepartmentDTO> list() throws BadRequestExceptions;
    DepartmentDTO findByCode(Long code) throws BadRequestExceptions;
    DepartmentDTO findByName(String name) throws BadRequestExceptions;
}
