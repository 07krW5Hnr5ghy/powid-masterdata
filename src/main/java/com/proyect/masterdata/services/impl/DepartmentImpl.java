package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.DepartmentMapper;
import com.proyect.masterdata.repository.DepartamentRepository;
import com.proyect.masterdata.services.IDepartment;
import lombok.AllArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import java.util.List;
@Service
@RequiredArgsConstructor
public class DepartmentImpl implements IDepartment {

    private final DepartamentRepository departamentRepository;

    private final DepartmentMapper departmentMapper;

    @Override
    public List<DepartmentDTO> listDepartment() throws BadRequestExceptions {
        departamentRepository.save(Department.builder().name("LORETO").build());
        departamentRepository.save(Department.builder().name("LIMA").build());
        return departmentMapper.departmentListToDepartmentDTOList(departamentRepository.findAll());
    }
}
