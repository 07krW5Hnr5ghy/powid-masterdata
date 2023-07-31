package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.ProvinceDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface DistrictMapper {
    DistrictMapper INSTANCE = Mappers.getMapper(DistrictMapper.class);

    @Mapping(target = "code", source = "id")
    DistrictDTO districtToDistrictDTO(District department);

    List<DistrictDTO> listDistrictToListDistrictDTO(List<District> departmentList);
}
