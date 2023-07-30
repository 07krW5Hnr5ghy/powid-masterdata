package com.proyect.masterdata.mapper;

import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import com.proyect.masterdata.domain.Module;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ModuleMapper {
    ModuleMapper INSTANCE = Mappers.getMapper(ModuleMapper.class);
    @Mapping(source="id",target = "id")
    @Mapping(source="name",target = "name")
    @Mapping(source = "status",target = "status")
    MasterListDTO moduleToModuleDTO(Module module);
    List<MasterListDTO> moduleListToModuleListDTO(List<Module> moduleList);
}
