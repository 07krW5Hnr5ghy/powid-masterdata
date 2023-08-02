package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.SizeType;
import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.request.RequestSize;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface SizeTypeMapper {
    SizeTypeMapper INSTANCE = Mappers.getMapper(SizeTypeMapper.class);
    @Mapping(source="code",target = "id")
    SizeTypeDTO sizeTypeToSizeTypeDTO(SizeType sizeType);
    List<SizeTypeDTO> sizeTypeListToSizeTypeListDTO(List<SizeType> sizeTypeList);
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    SizeType sizeTypeToName(String name);

    List<SizeType> sizeToListName(List<String> names);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "dateRegistration", ignore = true)
    SizeType requestSizeTypeToSize(RequestSize requestSize);

}
