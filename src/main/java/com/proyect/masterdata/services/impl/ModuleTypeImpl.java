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
        boolean existsModule;
        boolean existsUserType;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            existsModule = moduleRepository.existsByName(module.toUpperCase());
            existsUserType = userTypeRepository.existsByUserType(userType.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }

        if(!existsModule){
            throw new BadRequestExceptions("Modulo no existe");
        }

        if(!existsUserType){
            throw new BadRequestExceptions("Tipo de usuario no existe");
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
    public ResponseSuccess saveAll(List<RequestModuleTypeSave> requestModuleTypeSaveList, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        List<Module> modules;
        List<UserType> userTypes;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            modules = moduleRepository.findByNameIn(requestModuleTypeSaveList.stream().map(module -> module.getModule().toUpperCase()).collect(Collectors.toList()));
            userTypes = userTypeRepository.findByUserTypeIn(requestModuleTypeSaveList.stream().map(userType -> userType.getUserType().toUpperCase()).collect(Collectors.toList()));
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
        if(userTypes.isEmpty()){
            throw new BadRequestExceptions("Tipos de usuario no existen");
        }
        if(modules.size() != requestModuleTypeSaveList.size()){
            throw new BadRequestExceptions("Modulo no existe");
        }
        if(userTypes.size() != requestModuleTypeSaveList.size()){
            throw new BadRequestExceptions("Tipo de usuario no existe");
        }
        List<ModuleType> moduleTypeList = requestModuleTypeSaveList.stream().map(moduleType -> ModuleType.builder()
                .idModule(moduleRepository.findByNameAndStatusTrue(moduleType.getModule().toUpperCase()).getId())
                .idUserTypeModule(userTypeModuleRepository.findByUserTypeAndStatusTrue(moduleType.getUserType().toUpperCase()).getId())
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
        UserTypeModule userTypeModule;
        Module moduleData;
        try {
            userTypeModule = userTypeModuleRepository.findByUserType(userType.toUpperCase());
            moduleData = moduleRepository.findByNameAndStatusTrue(module.toUpperCase());
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
