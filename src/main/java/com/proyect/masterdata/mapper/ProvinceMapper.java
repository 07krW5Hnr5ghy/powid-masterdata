package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestProvince;
import com.proyect.masterdata.dto.request.RequestProvinceSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ProvinceMapper {
    ProvinceMapper INSTANCE = Mappers.getMapper(ProvinceMapper.class);

    @Mapping(target = "status", constant = "true")
    @Mapping(target = "registrationDate", ignore = true)
    @Mapping(target = "name", source = "requestProvinceSave.name")
    @Mapping(target = "departmentId", source = "requestProvinceSave.codeDepartment")
    Province provinceToName(RequestProvinceSave requestProvinceSave);

    @Mapping(target = "departmentId", source = "codeDepartment")
    @Mapping(target = "registrationDate", ignore = true)
    Province requestProvinceToProvince(RequestProvince requestProvince);

    @Mapping(target = "nameDepartment", source = "department.name")
    ProvinceDTO provinceToProvinceDTO(Province province);

    List<ProvinceDTO> listProvinceToListProvinceDTO(List<Province> provinceList);

    List<Province> listProvinceToListName(List<RequestProvinceSave> requestProvinceSaveList);
}
