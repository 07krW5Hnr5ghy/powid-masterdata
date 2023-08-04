package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.MembershipType;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.MembershipTypeDTO;
import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.dto.request.RequestMembershipType;
import com.proyect.masterdata.dto.request.RequestModule;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.ModuleMapper;
import com.proyect.masterdata.repository.ModuleRepository;
import com.proyect.masterdata.services.IMasterList;
import com.proyect.masterdata.services.IModule;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ModuleImpl implements IModule {
    private final ModuleRepository moduleRepository;
    private final ModuleMapper moduleMapper;

    @Override
    public ResponseSuccess save(String name) throws BadRequestExceptions {
        try {
            moduleRepository.save(moduleMapper.moduleToName(name.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions{
        try {
            moduleRepository.saveAll(moduleMapper.listModuleToListName(
                    names.stream().map(String::toUpperCase).collect(Collectors.toList())));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ModuleDTO update(RequestModule requestModule) throws BadRequestExceptions {
        try {
            requestModule.setName(requestModule.getName().toUpperCase());
            Module module = moduleRepository.save(moduleMapper.requestModuleToModule(requestModule));
            return moduleMapper.moduleToModuleDTO(module);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            moduleRepository.deleteById(code);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions{
        try {
            moduleRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<ModuleDTO> list() throws BadRequestExceptions{
        try {
            return moduleMapper.listModuleToListModuleDTO(moduleRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public ModuleDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return moduleMapper.moduleToModuleDTO(moduleRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public ModuleDTO findByName(String name) throws BadRequestExceptions{
        try {
            return moduleMapper.moduleToModuleDTO(moduleRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
