package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.ProvinceDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ProvinceMapper {
    ProvinceMapper INSTANCE = Mappers.getMapper(ProvinceMapper.class);

    @Mapping(target = "code", source = "id")
    ProvinceDTO provinceToProvinceDTO(Province department);

    List<ProvinceDTO> listProvinceToListProvinceDTO(List<Province> departmentList);
}
