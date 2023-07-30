package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ProvinceMapper {
    ProvinceMapper INSTANCE = Mappers.getMapper(ProvinceMapper.class);
    @Mapping(source="id",target = "id")
    @Mapping(source="name",target = "name")
    @Mapping(source = "status",target = "status")
    MasterListDTO provinceToProvinceDTO(Province province);
    List<MasterListDTO> provinceListToProvinceListDTO(List<Province> provinceList);
}
