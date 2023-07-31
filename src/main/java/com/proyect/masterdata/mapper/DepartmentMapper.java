package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface DepartmentMapper {
    DepartmentMapper INSTANCE = Mappers.getMapper(DepartmentMapper.class);

    @Mapping(target = "code", source = "id")
    DepartmentDTO departmentToDepartmentDTO(Department department);

    List<DepartmentDTO> listDepartmentToListDepartmentDTO(List<Department> departmentList);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", ignore = true)
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    Department departmentToName(String name);

    List<Department> listDepartmentToListName(List<String> names);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "status", ignore = true)
    @Mapping(target = "dateRegistration", ignore = true)
    Department requestDepartmentToDepartment(RequestDepartment requestDepartment);
}
