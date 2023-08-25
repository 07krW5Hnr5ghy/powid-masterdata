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

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "requestProvinceSave.name")
    @Mapping(target = "user", source = "requestProvinceSave.user")
    @Mapping(target = "idDepartment", source = "requestProvinceSave.codeDepártment")
    Province provinceToName(RequestProvinceSave requestProvinceSave);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "idDepartment", source = "codeDepartment")
    @Mapping(target = "dateRegistration", ignore = true)
    Province requestProvinceToProvince(RequestProvince requestProvince);

    @Mapping(target = "code", source = "id")
    @Mapping(target = "nameDepartment", source = "department.name")
    ProvinceDTO provinceToProvinceDTO(Province province);

    List<ProvinceDTO> listProvinceToListProvinceDTO(List<Province> provinceList);

    List<Province> listProvinceToListName(List<RequestProvinceSave> requestProvinceSaveList);
}
