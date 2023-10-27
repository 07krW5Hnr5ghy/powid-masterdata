package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.ModuleType;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.domain.UserType;
import com.proyect.masterdata.domain.UserTypeModule;
import com.proyect.masterdata.dto.ModuleTypeDTO;
import com.proyect.masterdata.dto.request.RequestModuleTypeSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IModuleType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class ModuleTypeImpl implements IModuleType {
    private final ModuleRepository moduleRepository;
    private final UserRepository userRepository;
    private final UserTypeRepository userTypeRepository;
    private final UserTypeModuleRepository userTypeModuleRepository;
    private final ModuleTypeRepository moduleTypeRepository;
    private final ModuleTypeRepositoryCustom moduleTypeRepositoryCustom;
    @Override
    public ResponseSuccess save(String userType, String module, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        Module moduleData;
        UserType userTypeData;
        ModuleType moduleType;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            moduleData = moduleRepository.findByNameAndStatusTrue(module.toUpperCase());
            userTypeData = userTypeRepository.findByUserTypeAndStatusTrue(userType.toUpperCase());
            moduleType = moduleTypeRepository.findByIdUserTypeModuleAndIdModule(userTypeData.getId(),moduleData.getId());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }

        if(moduleData==null){
            throw new BadRequestExceptions("Modulo no existe");
        }

        if(userType==null){
            throw new BadRequestExceptions("Tipo de usuario no existe");
        }

        if(moduleType!=null){
            throw new BadRequestExceptions("Tipo de modulo ya existe");
        }

        try{
            moduleTypeRepository.save(ModuleType.builder()
                            .status(true)
                            .dateRegistration(new Date(System.currentTimeMillis()))
                            .idModule(moduleRepository.findByNameAndStatusTrue(module.toUpperCase()).getId())
                            .idUserTypeModule(userTypeModuleRepository.findByUserTypeAndStatusTrue(userType.toUpperCase()).getId())
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(String userType,List<String> moduleList, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        List<Module> modules;
        UserType userTypeData;
        List<ModuleType> moduleTypes;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            modules = moduleRepository.findByNameIn(moduleList.stream().map(String::toUpperCase).toList());
            userTypeData = userTypeRepository.findByUserTypeAndStatusTrue(userType.toUpperCase());
            moduleTypes = moduleList.stream().map(module -> {
                ModuleType moduleType = moduleTypeRepository.findByIdUserTypeModuleAndIdModule(userTypeData.getId(),moduleRepository.findByNameAndStatusTrue(module.toUpperCase()).getId());
                return moduleType;
            }).toList();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(modules.isEmpty()){
            throw new BadRequestExceptions("Modulos no existen");
        }
        if(userTypeData==null){
            throw new BadRequestExceptions("Tipo de usuario no existen");
        }
        if(modules.size() != moduleList.size()){
            throw new BadRequestExceptions("Modulo no existe");
        }
        if(!moduleTypes.stream().filter(Objects::nonNull).toList().isEmpty()){
            throw new BadRequestExceptions("Tipo de modulo ya existe");
        }
        List<ModuleType> moduleTypeList = moduleList.stream().map(moduleType -> ModuleType.builder()
                .idModule(moduleRepository.findByNameAndStatusTrue(moduleType.toUpperCase()).getId())
                .idUserTypeModule(userTypeModuleRepository.findByUserTypeAndStatusTrue(userTypeData.getUserType()).getId())
                .dateRegistration(new Date(System.currentTimeMillis()))
                .status(true)
                .build()
        ).toList();
        try{
            moduleTypeRepository.saveAll(moduleTypeList);
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseDelete delete(String userType, String module, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        UserTypeModule userTypeModule;
        Module moduleData;
        ModuleType moduleType;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            userTypeModule = userTypeModuleRepository.findByUserType(userType.toUpperCase());
            moduleData = moduleRepository.findByNameAndStatusTrue(module.toUpperCase());
            moduleType = moduleTypeRepository.findByIdUserTypeModuleAndIdModule(userTypeModule.getId(),moduleData.getId());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(userTypeModule==null){
            throw new BadRequestExceptions("Tipo de usuario no existe");
        }
        if(moduleData==null){
            throw new BadRequestExceptions("Modulo no existe");
        }
        if(moduleType==null){
            throw new BadRequestExceptions("Tipo de usuario x Modulo no existe");
        }
        try {
            moduleTypeRepository.delete(moduleType);
            return ResponseDelete.builder()
                    .message(Constants.delete)
                    .code(200)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<ModuleTypeDTO> list(String userType, String module, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        Page<ModuleType> moduleTypePage = null;
        UserTypeModule userTypeModule = null;
        Module moduleData = null;
        try {
            if(userType != null){
                userTypeModule = userTypeModuleRepository.findByUserType(userType.toUpperCase());
            }
            if(module != null){
                moduleData = moduleRepository.findByNameAndStatusTrue(module.toUpperCase());
            }
            if(userTypeModule != null && moduleData != null){
                moduleTypePage = moduleTypeRepositoryCustom.searchForModuleType(userTypeModule.getId(), moduleData.getId(),sort,sortColumn,pageNumber,pageSize);
            }

            if(userTypeModule != null && moduleData == null){
                moduleTypePage = moduleTypeRepositoryCustom.searchForModuleType(userTypeModule.getId(),null,sort,sortColumn,pageNumber,pageSize);
            }

            if(userTypeModule == null && moduleData != null){
                moduleTypePage = moduleTypeRepositoryCustom.searchForModuleType(null, moduleData.getId(),sort,sortColumn,pageNumber,pageSize);
            }

            if(userTypeModule == null && moduleData == null){
                moduleTypePage = moduleTypeRepositoryCustom.searchForModuleType(null, null,sort,sortColumn,pageNumber,pageSize);
            }
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(moduleTypePage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        List<ModuleTypeDTO> moduleTypeDTOS = moduleTypePage.getContent().stream().map(moduleType -> {
            UserTypeModule innerUserTypeModule = userTypeModuleRepository.findById(moduleType.getIdUserTypeModule()).orElse(null);
            Module innerModule = moduleRepository.findById(moduleType.getIdModule()).orElse(null);
            return ModuleTypeDTO.builder()
                    .userType(innerUserTypeModule.getUserType())
                    .module(innerModule.getName())
                    .build();
        }).toList();
        return new PageImpl<>(moduleTypeDTOS,moduleTypePage.getPageable(),moduleTypePage.getTotalElements());
    }
}
