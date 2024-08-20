package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.UnitType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.UnitTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.UnitTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IUnitType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class UnitTypeImpl implements IUnitType {
    private final UserRepository userRepository;
    private final UnitTypeRepository unitTypeRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        UnitType unitType;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            unitType = unitTypeRepository.findByName(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(unitType != null){
            throw new BadRequestExceptions(Constants.ErrorUnitTypeExists);
        }

        try {
            UnitType newUnitType = unitTypeRepository.save(UnitType.builder()
                            .name(name.toUpperCase())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());
            iAudit.save("ADD_UNIT_TYPE","TIPO DE UNIDAD "+newUnitType.getName()+" CREADO.",newUnitType.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            UnitType unitType;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                unitType = unitTypeRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(unitType != null){
                throw new BadRequestExceptions(Constants.ErrorUnitTypeExists);
            }

            try {
                UnitType newUnitType = unitTypeRepository.save(UnitType.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_UNIT_TYPE","TIPO DE UNIDAD "+newUnitType.getName()+" CREADO.",newUnitType.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            UnitType unitType;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                unitType = unitTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(unitType == null){
                throw new BadRequestExceptions(Constants.ErrorUnitType);
            }

            try {
                unitType.setStatus(false);
                unitType.setUpdateDate(new Date(System.currentTimeMillis()));
                unitType.setTokenUser(user.getUsername());
                unitTypeRepository.save(unitType);
                iAudit.save("DELETE_UNIT_TYPE","TIPO DE UNIDAD "+unitType.getName()+" DESACTIVADO.",unitType.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            UnitType unitType;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                unitType = unitTypeRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(unitType == null){
                throw new BadRequestExceptions(Constants.ErrorUnitType);
            }

            try {
                unitType.setStatus(true);
                unitType.setUpdateDate(new Date(System.currentTimeMillis()));
                unitType.setTokenUser(user.getUsername());
                unitTypeRepository.save(unitType);
                iAudit.save("ACTIVATE_UNIT_TYPE","TIPO DE UNIDAD "+unitType.getName()+" ACTIVADA.",unitType.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<UnitTypeDTO>> listUnitType() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<UnitType> unitTypes;

            try{
                unitTypes = unitTypeRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(unitTypes.isEmpty()){
                return Collections.emptyList();
            }

            return unitTypes.stream().map(unitType -> UnitTypeDTO.builder()
                    .name(unitType.getName())
                    .build()).toList();
        });
    }
}
