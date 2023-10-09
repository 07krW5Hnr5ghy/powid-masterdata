package com.proyect.masterdata.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper(componentModel = "spring")
public interface ModuleTypeMapper {
    ModuleTypeMapper INSTANCE = Mappers.getMapper(ModuleTypeMapper.class);

}
