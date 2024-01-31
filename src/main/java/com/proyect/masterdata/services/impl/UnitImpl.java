package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.proyect.masterdata.domain.UnitType;
import com.proyect.masterdata.dto.request.RequestUnit;
import com.proyect.masterdata.repository.UnitTypeRepository;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Unit;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.UnitDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.UnitRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IUnit;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class UnitImpl implements IUnit {

    private final UserRepository userRepository;
    private final UnitRepository unitRepository;
    private final UnitTypeRepository unitTypeRepository;

    @Override
    public ResponseSuccess save(RequestUnit requestUnit, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Unit unit;
        UnitType unitType;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            unit = unitRepository.findByNameAndStatusTrue(requestUnit.getName().toUpperCase());
            unitType = unitTypeRepository.findByNameAndStatusTrue(requestUnit.getUnitType().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (unit != null) {
            throw new BadRequestExceptions(Constants.ErrorUnitExists);
        }

        if(unitType == null){
            throw new BadRequestExceptions(Constants.ErrorUnitType);
        }

        try {
            unitRepository.save(Unit.builder()
                    .name(requestUnit.getName().toUpperCase())
                            .unitType(unitType)
                            .unitTypeId(unitType.getId())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .tokenUser(tokenUser.toUpperCase())
                    .build());

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
    public ResponseSuccess saveAll(List<String> names,String unitType, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        List<Unit> units;
        UnitType unitTypeData;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            units = unitRepository.findByNameInAndStatusTrue(names.stream().map(String::toUpperCase).toList());
            unitTypeData = unitTypeRepository.findByNameAndStatusTrue(unitType.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (!units.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorUnitExists);
        }

        if(unitTypeData == null){
            throw new BadRequestExceptions(Constants.ErrorUnitType);
        }

        try {
            unitRepository.saveAll(names.stream().map(name -> Unit.builder()
                    .name(name.toUpperCase())
                    .unitType(unitTypeData)
                    .unitTypeId(unitTypeData.getId())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .tokenUser(tokenUser)
                    .build()).toList());

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
    public ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Unit unit;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            unit = unitRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user != null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (unit == null) {
            throw new BadRequestExceptions(Constants.ErrorUnit);
        }

        try {
            unit.setStatus(false);
            unit.setUpdateDate(new Date(System.currentTimeMillis()));
            unitRepository.save(unit);

            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();

        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<UnitDTO> listUnit() throws BadRequestExceptions {

        List<Unit> units;

        try {
            units = unitRepository.findAllByStatusTrue();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (units.isEmpty()) {
            return Collections.emptyList();
        }

        return units.stream().map(unit -> UnitDTO.builder()
                .name(unit.getName())
                .unitType(unit.getUnitType().getName())
                .build()).toList();
    }

}
