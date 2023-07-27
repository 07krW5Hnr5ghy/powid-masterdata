package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.response.ResponseDepartment;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import java.util.List;

public interface IDepartment {

    ResponseDepartment createDepartment(String name)throws BadRequestExceptions;
    List<DepartmentDTO> listDepartment() throws BadRequestExceptions;
    DepartmentDTO update(Long code, String name) throws BadRequestExceptions;
    ResponseDepartment deleteDepartment(Long code)throws BadRequestExceptions;

}
