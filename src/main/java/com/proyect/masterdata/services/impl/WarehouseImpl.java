package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.WarehouseDTO;
import com.proyect.masterdata.dto.request.RequestWarehouse;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.repository.WarehouseRepositoryCustom;
import com.proyect.masterdata.services.IWarehouse;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class WarehouseImpl implements IWarehouse {

    private final UserRepository userRepository;
    private final WarehouseRepository warehouseRepository;
    private final WarehouseRepositoryCustom warehouseRepositoryCustom;

    @Override
    public ResponseSuccess save(RequestWarehouse requestWarehouse, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Warehouse warehouse;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouse = warehouseRepository.findByName(requestWarehouse.getName().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (warehouse != null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouseExists);
        }

        try {
            warehouseRepository.save(Warehouse.builder()
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .location(requestWarehouse.getLocation().toUpperCase())
                    .name(requestWarehouse.getName().toUpperCase())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
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
    public ResponseSuccess saveAll(List<RequestWarehouse> requestWarehousesList, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        List<Warehouse> warehouseList;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouseList = warehouseRepository.findByNameIn(
                    requestWarehousesList.stream().map(warehouse -> warehouse.getName().toUpperCase()).toList());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (!warehouseList.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorWarehouseExists);
        }

        try {
            warehouseRepository.saveAll(requestWarehousesList.stream().map(warehouse -> Warehouse.builder()
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .location(warehouse.getLocation().toUpperCase())
                    .name(warehouse.getName().toUpperCase())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(tokenUser.toUpperCase())
                    .build()).toList());

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
    public Page<WarehouseDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        Page<Warehouse> warehousePage;
        Long clientId;

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            warehousePage = warehouseRepositoryCustom.searchForWarehouse(name, clientId, sort, sortColumn, pageNumber,
                    pageSize, true);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (warehousePage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<WarehouseDTO> warehouseDTOs = warehousePage.getContent().stream().map(warehouse -> WarehouseDTO.builder()
                .location(warehouse.getLocation())
                .name(warehouse.getName())
                .build()).toList();

        return new PageImpl<>(warehouseDTOs, warehousePage.getPageable(), warehousePage.getTotalElements());
    }

    @Override
    public List<WarehouseDTO> listWarehouse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        List<Warehouse> warehouses;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            warehouses = warehouseRepository.findAllByClientIdAndStatusTrue(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (warehouses.isEmpty()){
            return Collections.emptyList();
        }

        return warehouses.stream().map(warehouse -> WarehouseDTO.builder()
                .location(warehouse.getLocation())
                .name(warehouse.getName())
                .build()).toList();
    }

    @Override
    public List<WarehouseDTO> listWarehouseFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        List<Warehouse> warehouses;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            warehouses = warehouseRepository.findAllByClientIdAndStatusFalse(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (warehouses.isEmpty()){
            return Collections.emptyList();
        }

        return warehouses.stream().map(warehouse -> WarehouseDTO.builder()
                .location(warehouse.getLocation())
                .name(warehouse.getName())
                .build()).toList();
    }

}
