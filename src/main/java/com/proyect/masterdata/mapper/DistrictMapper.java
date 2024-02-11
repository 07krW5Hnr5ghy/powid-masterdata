package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.request.RequestDistrict;
import com.proyect.masterdata.dto.request.RequestDistrictSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface DistrictMapper {
    DistrictMapper INSTANCE = Mappers.getMapper(DistrictMapper.class);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "registrationDate", ignore = true)
    @Mapping(target = "name", source = "requestDistrictSave.name")
    @Mapping(target = "tokenUser", source = "requestDistrictSave.user")
    @Mapping(target = "provinceId", source = "requestDistrictSave.codeProvince")
    District districtToName(RequestDistrictSave requestDistrictSave);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "provinceId", source = "codeProvince")
    @Mapping(target = "registrationDate", ignore = true)
    District requestDistrictToDistrict(RequestDistrict requestDistrict);

    @Mapping(target = "nameProvince", source = "province.name")
    DistrictDTO districtToDistrictDTO(District district);

    List<DistrictDTO> listDistrictToListDistrictDTO(List<District> districtList);

    List<District> listDistrictToListName(List<RequestDistrictSave> requestDistrictSaveList);

}
