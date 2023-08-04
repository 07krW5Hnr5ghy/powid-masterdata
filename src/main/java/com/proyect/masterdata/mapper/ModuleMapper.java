package com.proyect.masterdata.mapper;

import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.dto.request.RequestModule;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import com.proyect.masterdata.domain.Module;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ModuleMapper {
//    ModuleMapper INSTANCE = Mappers.getMapper(ModuleMapper.class);
//    @Mapping(target = "code", source = "id")
//    ModuleDTO moduleToModuleDTO(Module module);
//    List<ModuleDTO> listModuleToListModuleDTO(List<Module> moduleList);
//    @Mapping(target = "id", ignore = true)
//    @Mapping(target = "status", constant = "true")
//    @Mapping(target = "dateRegistration", ignore = true)
//    @Mapping(target = "name", source = "name")
//    Module moduleToName(String name);
//
//    List<Module> listModuleToListName(List<String> names);
//
//    @Mapping(target = "id", source = "code")
//    @Mapping(target = "dateRegistration", ignore = true)
//    Module requestModuleToModule(RequestModule requestModule);
}
