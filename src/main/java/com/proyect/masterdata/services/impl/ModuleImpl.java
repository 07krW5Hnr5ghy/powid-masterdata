package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.dto.request.RequestModule;
import com.proyect.masterdata.dto.request.RequestModuleSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ModuleMapper;
import com.proyect.masterdata.repository.ModuleRepository;
import com.proyect.masterdata.repository.ModuleRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IModule;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class ModuleImpl implements IModule {
    private final ModuleRepository moduleRepository;
    private final ModuleRepositoryCustom moduleRepositoryCustom;
    private final ModuleMapper moduleMapper;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name, double price, int moduleStatus, String user)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsModule;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            existsModule = moduleRepository.existsByName(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario incorrecto");
        }
        if (existsModule) {
            throw new BadRequestExceptions("El modulo ya existe");
        }

        try {
            moduleRepository.save(moduleMapper.moduleToName(RequestModuleSave.builder()
                    .name(name.toUpperCase())
                    .moduleStatus(moduleStatus)
                    .price(price)
                    .status(true)
                    .build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<RequestModuleSave> moduleList, String user)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        List<Module> modules;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            modules = moduleRepository.findByNameIn(
                    moduleList.stream().map(module -> module.getName().toUpperCase()).collect(Collectors.toList()));
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (!modules.isEmpty()) {
            throw new BadRequestExceptions("modulo ya existente");
        }
        List<RequestModuleSave> moduleSaveList = moduleList.stream().map(module -> RequestModuleSave.builder()
                .name(module.getName().toUpperCase())
                .price(module.getPrice())
                .status(true)
                .moduleStatus(module.getModuleStatus())
                .build()).toList();
        try {
            moduleRepository.saveAll(moduleMapper.listModuleToListName(moduleSaveList));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ModuleDTO update(RequestModule requestModule) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        Module module;
        try {
            existsUser = userRepository.existsByUsername(requestModule.getUser().toUpperCase());
            module = moduleRepository.findByNameAndStatusTrue(requestModule.getName().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (module == null) {
            throw new BadRequestExceptions("Modulo no existe");
        }
        module.setPrice(requestModule.getPrice());
        module.setStatus_module(requestModule.getStatusModule());
        module.setStatus(requestModule.isStatus());
        module.setDateRegistration(new Date(System.currentTimeMillis()));

        try {
            return moduleMapper.moduleToModuleDTO(moduleRepository.save(module));
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        Module module;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            module = moduleRepository.findById(code).orElse(null);
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (module == null) {
            throw new BadRequestExceptions("Modulo no existe");
        }
        module.setStatus(false);
        module.setDateRegistration(new Date(System.currentTimeMillis()));
        try {
            moduleRepository.save(module);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<ModuleDTO> listModule() {
        List<Module> modules;
        try {
            modules = moduleRepository.findAllByStatusTrue();
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (modules.isEmpty()) {
            return Collections.emptyList();
        }
        return moduleMapper.listModuleToListModuleDTO(modules);
    }

    @Override
    public Page<ModuleDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        Page<Module> modulePage;
        try {
            modulePage = moduleRepositoryCustom.searchForModule(name, user, sort, sortColumn, pageNumber, pageSize,
                    true);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (modulePage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(moduleMapper.listModuleToListModuleDTO(modulePage.getContent()),
                modulePage.getPageable(), modulePage.getTotalElements());
    }

    @Override
    public Page<ModuleDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        Page<Module> modulePage;
        try {
            modulePage = moduleRepositoryCustom.searchForModule(name, user, sort, sortColumn, pageNumber, pageSize,
                    false);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (modulePage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(moduleMapper.listModuleToListModuleDTO(modulePage.getContent()),
                modulePage.getPageable(), modulePage.getTotalElements());
    }

    @Override
    public ModuleDTO findByCode(Long code) throws BadRequestExceptions {
        try {
            return moduleMapper.moduleToModuleDTO(moduleRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
