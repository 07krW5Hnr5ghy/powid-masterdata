package com.proyect.masterdata.mapper;

import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.domain.Module;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ModuleMapper {
    ModuleMapper INSTANCE = Mappers.getMapper(ModuleMapper.class);

    @Mapping(target = "moduleName", source = "name")
    @Mapping(target = "modulePrice", source = "monthlyPrice")
    ModuleDTO moduleToModuleDTO(Module module);

    List<ModuleDTO> listModuleToListModuleDTO(List<Module> moduleList);
}
