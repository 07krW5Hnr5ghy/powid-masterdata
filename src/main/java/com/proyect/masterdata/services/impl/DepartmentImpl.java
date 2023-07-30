package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.response.ResponseDepartment;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.DepartmentMapper;
import com.proyect.masterdata.repository.DepartamentRepository;
import com.proyect.masterdata.services.IDepartment;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import java.util.List;
@Service
@RequiredArgsConstructor
public class DepartmentImpl implements IDepartment {

    private final DepartamentRepository departamentRepository;

    private final DepartmentMapper departmentMapper;

    @Override
    public ResponseDepartment createDepartment(String name) throws BadRequestExceptions {
        try {
            departamentRepository.save(Department.builder().name(name).build());
            return ResponseDepartment.builder()
                    .code(200)
                    .message("Success")
                    .build();
        } catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public List<DepartmentDTO> listDepartment() throws BadRequestExceptions {
        return departmentMapper.departmentListToDepartmentDTOList(departamentRepository.findAll());
    }

    @Override
    public DepartmentDTO update(Long code, String name) throws BadRequestExceptions {
        try {
            Department department = departamentRepository.save(Department.builder().codeDepartment(code).name(name).build());
            return departmentMapper.departmentToDepartmentDTO(department);
        } catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseDepartment deleteDepartment(Long code) throws BadRequestExceptions {
        return null;
    }
}
