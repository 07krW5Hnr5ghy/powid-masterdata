package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestDepartmentSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import java.util.List;

@Mapper(componentModel = "spring")
public interface DepartmentMapper {
    DepartmentMapper INSTANCE = Mappers.getMapper(DepartmentMapper.class);

    DepartmentDTO departmentToDepartmentDTO(Department department);

    List<DepartmentDTO> listDepartmentToListDepartmentDTO(List<Department> departmentList);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "requestDepartmentSave.name")
    @Mapping(target = "tokenUser", source = "requestDepartmentSave.user")
    Department departmentToName(RequestDepartmentSave requestDepartmentSave);

    List<Department> listDepartmentToListName(List<RequestDepartmentSave> requestDepartmentSaveList);
}
