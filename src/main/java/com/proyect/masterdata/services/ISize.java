package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.request.RequestSize;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface ISize {
    ResponseSuccess save(String name) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions;
    SizeDTO update(RequestSize requestSize) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<SizeDTO> list() throws BadRequestExceptions;
    SizeDTO findByCode(Long code) throws BadRequestExceptions;
    SizeDTO findByName(String name) throws BadRequestExceptions;
}
