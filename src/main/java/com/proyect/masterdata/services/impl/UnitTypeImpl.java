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
import com.proyect.masterdata.services.IUnitType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class UnitTypeImpl implements IUnitType {
    private final UserRepository userRepository;
    private final UnitTypeRepository unitTypeRepository;
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
            unitTypeRepository.save(UnitType.builder()
                            .name(name.toUpperCase())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
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
    public ResponseSuccess saveAll(List<String> names, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try{
            unitTypeRepository.saveAll(names.stream().map(name -> {

                UnitType unitType = unitTypeRepository.findByName(name.toUpperCase());

                if(unitType != null){
                    throw new BadRequestExceptions(Constants.ErrorUnitTypeExists);
                }

                return UnitType.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
                        .build();
            }).toList());

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
    public ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

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
            unitTypeRepository.save(unitType);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<UnitTypeDTO> listUnitType() throws BadRequestExceptions {

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
    }
}
