package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;

import java.util.List;

public interface IDepartment {
    ResponseSuccess save(String name);
    ResponseSuccess saveAll(List<String> names);
    DepartmentDTO update(Long code, String name);
    ResponseDelete delete(Long code);
    ResponseDelete deleteAll(List<Long> codes);
    List<DepartmentDTO> list();
    DepartmentDTO findByCode(Long code);
    DepartmentDTO findByName(String name);
}
