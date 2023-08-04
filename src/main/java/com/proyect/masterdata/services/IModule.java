package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.request.RequestModule;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IModule {
    ResponseSuccess save(String name) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions;
    ModuleDTO update(RequestModule requestModule) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<ModuleDTO> list() throws BadRequestExceptions;
    ModuleDTO findByCode(Long code) throws BadRequestExceptions;
    ModuleDTO findByName(String name) throws BadRequestExceptions;
}
