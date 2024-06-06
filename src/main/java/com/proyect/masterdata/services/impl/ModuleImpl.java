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
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(tokenUser.toUpperCase())
                    .build());
            iAudit.save("ADD_MODULE","ADD MODULE "+newModule.getName()+".",user.getUsername());
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
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .tokenUser(tokenUser.toUpperCase())
                        .build());
                iAudit.save("ADD_MODULE","ADD MODULE "+newModule.getName()+".",user.getUsername());
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

            module.setMonthlyPrice(requestModule.getMontlyPrice());
            module.setUpdateDate(new Date(System.currentTimeMillis()));
            iAudit.save("UPDATE_MODULE","UPDATE MODULE "+module.getName()+" WITH PRICE "+module.getMonthlyPrice()+".",user.getUsername());
            try {
                return moduleMapper.moduleToModuleDTO(moduleRepository.save(module));
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
                module.setUpdateDate(new Date(System.currentTimeMillis()));
                module.setTokenUser(user.getUsername());
                moduleRepository.save(module);
                iAudit.save("DELETE_MODULE","DELETE MODULE "+module.getName()+".",user.getUsername());
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
                module.setTokenUser(user.getUsername());
                module.setUpdateDate(new Date(System.currentTimeMillis()));
                moduleRepository.save(module);
                iAudit.save("ACTIVATE_MODULE","ACTIVATE MODULE "+module.getName()+".",user.getUsername());
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
            return new PageImpl<>(moduleMapper.listModuleToListModuleDTO(modulePage.getContent()),
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
            return new PageImpl<>(moduleMapper.listModuleToListModuleDTO(modulePage.getContent()),
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

            return moduleMapper.listModuleToListModuleDTO(modules);
        });
    }
}
