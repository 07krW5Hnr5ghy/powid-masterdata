package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.dto.request.RequestModule;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ModuleMapper;
import com.proyect.masterdata.repository.ModuleRepository;
import com.proyect.masterdata.repository.ModuleRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IModule;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class ModuleImpl implements IModule {
    private final ModuleRepository moduleRepository;
    private final ModuleRepositoryCustom moduleRepositoryCustom;
    private final ModuleMapper moduleMapper;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, double price, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        boolean existsModule;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            existsModule = moduleRepository.existsByName(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user==null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (existsModule) {
            throw new BadRequestExceptions(Constants.ErrorModuleExist);
        }

        try {

            Module newModule = moduleRepository.save(Module.builder()
                    .name(name.toUpperCase())
                    .monthlyPrice(price)
                    .registrationDate(OffsetDateTime.now())
                    .status(true)
                    .user(user)
                    .userId(user.getId())
                    .build());
            iAudit.save("ADD_MODULE","MODULO "+newModule.getName()+".",newModule.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, double price, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            boolean existsModule;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                existsModule = moduleRepository.existsByName(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (existsModule) {
                throw new BadRequestExceptions(Constants.ErrorModuleExist);
            }

            try {

                Module newModule = moduleRepository.save(Module.builder()
                        .name(name.toUpperCase())
                        .monthlyPrice(price)
                        .registrationDate(OffsetDateTime.now())
                        .status(true)
                        .user(user)
                        .userId(user.getId())
                        .build());
                iAudit.save("ADD_MODULE","MODULO "+newModule.getName()+" CREADO.",newModule.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();

            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ModuleDTO> update(RequestModule requestModule, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Module module;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                module = moduleRepository.findByNameAndStatusTrue(requestModule.getName().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (module == null) {
                throw new BadRequestExceptions(Constants.ErrorModule);
            }

            try {
                module.setMonthlyPrice(requestModule.getMontlyPrice());
                module.setUpdateDate(OffsetDateTime.now());
                iAudit.save("UPDATE_MODULE","MODULO ACTUALIZADO "+module.getName()+" CON PRECIO "+module.getMonthlyPrice()+".",module.getName(),user.getUsername());
                return ModuleDTO.builder()
                        .id(module.getId())
                        .moduleName(module.getName())
                        .modulePrice(module.getMonthlyPrice())
                        .status(module.getStatus())
                        .user(module.getUser().getUsername())
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Module module;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                module = moduleRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (module == null) {
                throw new BadRequestExceptions(Constants.ErrorModule);
            }

            try {
                module.setStatus(false);
                module.setUpdateDate(OffsetDateTime.now());
                module.setUser(user);
                module.setUserId(user.getId());
                moduleRepository.save(module);
                iAudit.save("DELETE_MODULE","MODULO "+module.getName()+" DESACTIVADO.",module.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();

            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Module module;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                module = moduleRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (module == null) {
                throw new BadRequestExceptions(Constants.ErrorModule);
            }

            try {
                module.setStatus(true);
                module.setUser(user);
                module.setUserId(user.getId());
                module.setUpdateDate(OffsetDateTime.now());
                moduleRepository.save(module);
                iAudit.save("ACTIVATE_MODULE","MODULO "+module.getName()+" ACTIVADO.",module.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<ModuleDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
            List<ModuleDTO> moduleDTOS = modulePage.getContent().stream().map(module -> ModuleDTO.builder()
                    .id(module.getId())
                    .moduleName(module.getName())
                    .modulePrice(module.getMonthlyPrice())
                    .status(module.getStatus())
                    .user(module.getUser().getUsername())
                    .build()).toList();
            return new PageImpl<>(moduleDTOS,
                    modulePage.getPageable(), modulePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ModuleDTO>> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
            List<ModuleDTO> moduleDTOS = modulePage.getContent().stream().map(module -> ModuleDTO.builder()
                    .id(module.getId())
                    .moduleName(module.getName())
                    .modulePrice(module.getMonthlyPrice())
                    .status(module.getStatus())
                    .user(module.getUser().getUsername())
                    .build()).toList();
            return new PageImpl<>(moduleDTOS,
                    modulePage.getPageable(), modulePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<ModuleDTO>> listModule() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Module> modules = new ArrayList<>();

            try {
                modules = moduleRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (modules.isEmpty()) {
                return Collections.emptyList();
            }
            return modules.stream().map(module -> ModuleDTO.builder()
                    .id(module.getId())
                    .moduleName(module.getName())
                    .modulePrice(module.getMonthlyPrice())
                    .status(module.getStatus())
                    .user(module.getUser().getUsername())
                    .build()).toList();
        });
    }
}
