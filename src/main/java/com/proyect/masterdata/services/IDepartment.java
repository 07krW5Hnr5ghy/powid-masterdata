package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import java.util.List;

public interface IDepartment {
    List<DepartmentDTO> listDepartment() throws BadRequestExceptions;
}
