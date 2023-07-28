package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.dto.DepartmentDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import java.util.List;

@Mapper(componentModel = "spring")
public interface DepartmentMapper {
    DepartmentMapper INSTANCE = Mappers.getMapper( DepartmentMapper.class );

    @Mapping(source = "codeDepartment", target = "code")
    DepartmentDTO departmentToDepartmentDTO(Department department);

    List<DepartmentDTO> departmentListToDepartmentDTOList(List<Department> departmentList);

}
