package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface DistrictMapper {
    DistrictMapper INSTANCE = Mappers.getMapper(DistrictMapper.class);
    @Mapping(source="id",target = "id")
    @Mapping(source = "name", target = "name")
    @Mapping(source = "status", target = "status")
    MasterListDTO districtToDistrictDTO(District district);

    List<MasterListDTO> districtListToDistrictListDTO(List<District> districtList);
}
