package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.SizeType;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface SizeTypeMapper {
    SizeTypeMapper INSTANCE = Mappers.getMapper(SizeTypeMapper.class);
    @Mapping(source = "id",target = "id")
    MasterListDTO sizeTypeToSizeTypeDTO(SizeType sizeType);
    List<MasterListDTO> sizeTypeListToSizeTypeListDTO(List<SizeType> sizeTypeList);

}
