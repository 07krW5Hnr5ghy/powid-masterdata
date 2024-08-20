package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.UnitType;
import com.proyect.masterdata.dto.request.RequestUnit;
import com.proyect.masterdata.repository.UnitTypeRepository;
import com.proyect.masterdata.services.IAudit;
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
    private final IAudit iAudit;
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
            Unit newUnit = unitRepository.save(Unit.builder()
                    .name(requestUnit.getName().toUpperCase())
                    .unitType(unitType)
                    .unitTypeId(unitType.getId())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .updateDate(new Date(System.currentTimeMillis()))
                    .tokenUser(tokenUser.toUpperCase())
                    .build());
            iAudit.save("ADD_UNIT","UNIDAD "+newUnit.getName()+" CREADA.",newUnit.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestUnit requestUnit, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                Unit newUnit = unitRepository.save(Unit.builder()
                        .name(requestUnit.getName().toUpperCase())
                        .unitType(unitType)
                        .unitTypeId(unitType.getId())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(tokenUser.toUpperCase())
                        .build());
                iAudit.save("ADD_UNIT","UNIDAD "+newUnit.getName()+" CREADA.",newUnit.getName(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Unit unit;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                unit = unitRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (unit == null) {
                throw new BadRequestExceptions(Constants.ErrorUnit);
            }

            try {
                unit.setStatus(false);
                unit.setUpdateDate(new Date(System.currentTimeMillis()));
                unit.setTokenUser(user.getUsername());
                unitRepository.save(unit);
                iAudit.save("DELETE_UNIT","UNIDAD "+unit.getName()+" DESACTIVADA.",unit.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();

            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Unit unit;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                unit = unitRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (unit == null) {
                throw new BadRequestExceptions(Constants.ErrorUnit);
            }

            try {
                unit.setStatus(true);
                unit.setUpdateDate(new Date(System.currentTimeMillis()));
                unit.setTokenUser(user.getUsername());
                unitRepository.save(unit);
                iAudit.save("ACTIVATE_UNIT","UNIDAD "+unit.getName()+" ACTIVADA.",unit.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();

            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<UnitDTO>> listUnit() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
        });
    }

    @Override
    public CompletableFuture<List<UnitDTO>> listUnitByType(String unitTypeName) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Unit> units;
            Long unitTypeId;

            try {
                unitTypeId = unitTypeRepository.findByNameAndStatusTrue(unitTypeName.toUpperCase()).getId();
                units = unitRepository.findAllByUnitTypeIdAndStatusTrue(unitTypeId);
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
        });
    }

    @Override
    public CompletableFuture<List<UnitDTO>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Unit> units;

            try {
                units = unitRepository.findAll();
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
        });
    }

}
