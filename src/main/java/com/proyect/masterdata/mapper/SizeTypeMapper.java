package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.SizeType;
import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.request.RequestSize;
import com.proyect.masterdata.dto.request.RequestSizeType;
import com.proyect.masterdata.dto.request.RequestSizeTypeSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface SizeTypeMapper {
    SizeTypeMapper INSTANCE = Mappers.getMapper(SizeTypeMapper.class);
    @Mapping(target = "code", source = "id")
    SizeTypeDTO sizeTypeToSizeTypeDTO(SizeType sizeType);
    List<SizeTypeDTO> listSizeTypeToListSizeTypeDTO(List<SizeType> sizeTypeList);
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    SizeType sizeTypeToName(String name,String user);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "dateRegistration", ignore = true)
    SizeType requestSizeTypeToSizeType(RequestSizeType requestSizeType);

    List<SizeType> listSizeToListName(List<RequestSizeTypeSave> requestSizeTypeSaveList);
}
