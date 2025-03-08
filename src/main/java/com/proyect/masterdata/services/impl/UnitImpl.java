package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.UUID;
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
            unitType = unitTypeRepository.findByNameAndStatusTrue(requestUnit.getUnitType().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(unitType == null){
            throw new BadRequestExceptions(Constants.ErrorUnitType);
        }else{
            unit = unitRepository.findByNameAndUnitTypeIdAndClientIdAndStatusTrue(requestUnit.getName().toUpperCase(), unitType.getId(),user.getClientId());
        }

        if (unit != null) {
            throw new BadRequestExceptions(Constants.ErrorUnitExists);
        }

        try {
            Unit newUnit = unitRepository.save(Unit.builder()
                    .name(requestUnit.getName().toUpperCase())
                    .unitType(unitType)
                    .unitTypeId(unitType.getId())
                    .status(true)
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .user(user).userId(user.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                    .build());
            iAudit.save("ADD_UNIT","UNIDAD "+newUnit.getName()+" CREADA.",newUnit.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            e.printStackTrace();
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
                unitType = unitTypeRepository.findByNameAndStatusTrue(requestUnit.getUnitType().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(unitType == null){
                throw new BadRequestExceptions(Constants.ErrorUnitType);
            }else{
                unit = unitRepository.findByNameAndUnitTypeIdAndClientIdAndStatusTrue(requestUnit.getName().toUpperCase(), unitType.getId(),user.getClientId());
            }

            if (unit != null) {
                throw new BadRequestExceptions(Constants.ErrorUnitExists);
            }

            try {
                Unit newUnit = unitRepository.save(Unit.builder()
                        .name(requestUnit.getName().toUpperCase())
                        .unitType(unitType)
                        .unitTypeId(unitType.getId())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .user(user).userId(user.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .build());
                iAudit.save("ADD_UNIT","UNIDAD "+newUnit.getName()+" CREADA.",newUnit.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name,String unitTypeName, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            UnitType unitType;
            Unit unit;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                unitType = unitTypeRepository.findByNameAndStatusTrue(unitTypeName.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(unitType == null){
                throw new BadRequestExceptions(Constants.ErrorUnitType);
            }else{
                unit = unitRepository.findByNameAndUnitTypeIdAndClientIdAndStatusTrue(name.toUpperCase(), unitType.getId(),user.getClientId());
            }

            if (unit != null) {
                throw new BadRequestExceptions(Constants.ErrorUnitExists);
            }

            try {
                unit.setStatus(false);
                unit.setUpdateDate(OffsetDateTime.now());
                unit.setUser(user);
                unit.setUserId(user.getId());
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
    public CompletableFuture<ResponseSuccess> activate(String name,String unitTypeName, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            UnitType unitType;
            Unit unit;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                unitType = unitTypeRepository.findByNameAndStatusTrue(unitTypeName.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(unitType == null){
                throw new BadRequestExceptions(Constants.ErrorUnitType);
            }else{
                unit = unitRepository.findByNameAndUnitTypeIdAndClientIdAndStatusTrue(name.toUpperCase(),unitType.getId(),user.getClientId());
            }

            if (unit == null) {
                throw new BadRequestExceptions(Constants.ErrorUnit);
            }

            try {
                unit.setStatus(true);
                unit.setUpdateDate(OffsetDateTime.now());
                unit.setUser(user);
                unit.setUserId(user.getId());
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
    public CompletableFuture<List<UnitDTO>> listUnit(String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Unit> units;
            User user;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                units = unitRepository.findAllByClientIdAndStatusTrue(user.getClientId());
            }

            if (units.isEmpty()) {
                return Collections.emptyList();
            }

            return units.stream().map(unit -> UnitDTO.builder()
                    .id(unit.getId())
                    .user(unit.getUser().getUsername())
                    .registrationDate(unit.getRegistrationDate())
                    .updateDate(unit.getUpdateDate())
                    .status(unit.getStatus())
                    .name(unit.getName())
                    .unitType(unit.getUnitType().getName())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<UnitDTO>> listUnitByType(String unitTypeName,String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Unit> units;
            UUID unitTypeId;
            User user;
            try {
                unitTypeId = unitTypeRepository.findByNameAndStatusTrue(unitTypeName.toUpperCase()).getId();
                user=userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                units = unitRepository.findAllByUnitTypeIdAndClientIdAndStatusTrue(unitTypeId,user.getClientId());
            }
            if (units.isEmpty()) {
                return Collections.emptyList();
            }

            return units.stream().map(unit -> UnitDTO.builder()
                    .id(unit.getId())
                    .user(unit.getUser().getUsername())
                    .registrationDate(unit.getRegistrationDate())
                    .updateDate(unit.getUpdateDate())
                    .status(unit.getStatus())
                    .name(unit.getName())
                    .unitType(unit.getUnitType().getName())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<UnitDTO>> listFilter(String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Unit> units;
            User user;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                units = unitRepository.findAllByClientId(user.getClientId());
            }

            if (units.isEmpty()) {
                return Collections.emptyList();
            }

            return units.stream().map(unit -> UnitDTO.builder()
                    .id(unit.getId())
                    .user(unit.getUser().getUsername())
                    .registrationDate(unit.getRegistrationDate())
                    .updateDate(unit.getUpdateDate())
                    .status(unit.getStatus())
                    .name(unit.getName())
                    .unitType(unit.getUnitType().getName())
                    .build()).toList();
        });
    }

}
