package com.proyect.masterdata.mapper;

import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.dto.request.RequestModuleSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ModuleMapper {
    ModuleMapper INSTANCE = Mappers.getMapper(ModuleMapper.class);

    @Mapping(target = "code", source = "id")
    @Mapping(target = "moduleName", source = "name")
    @Mapping(target = "modulePrice", source = "monthlyPrice")
    ModuleDTO moduleToModuleDTO(Module module);

    List<ModuleDTO> listModuleToListModuleDTO(List<Module> moduleList);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "name", source = "requestModuleSave.name")
    @Mapping(target = "price", source = "requestModuleSave.price")
    @Mapping(target = "status_module", source = "requestModuleSave.moduleStatus")
    @Mapping(target = "status")
    @Mapping(target = "dateRegistration", ignore = true)
    Module moduleToName(RequestModuleSave requestModuleSave);

    List<Module> listModuleToListName(List<RequestModuleSave> requestModuleSaveList);
}
