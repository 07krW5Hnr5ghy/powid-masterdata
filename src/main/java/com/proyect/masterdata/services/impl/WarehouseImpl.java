package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.UUID;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.services.IAudit;
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
    private final IAudit iAudit;
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
            Warehouse newWarehouse = warehouseRepository.save(Warehouse.builder()
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .phone(requestWarehouse.getPhone())
                    .address(requestWarehouse.getAddress())
                    .contact(requestWarehouse.getContact().toUpperCase())
                    .reference(requestWarehouse.getReference().toUpperCase())
                    .name(requestWarehouse.getName().toUpperCase())
                    .registrationDate(OffsetDateTime.now())
                    .status(true)
                    .user(user).userId(user.getId())
                    .build());
            iAudit.save("ADD_WAREHOUSE","ALMACEN "+newWarehouse.getName()+" CREADO.",newWarehouse.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(RequestWarehouse requestWarehouse, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                Warehouse newWarehouse = warehouseRepository.save(Warehouse.builder()
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .phone(requestWarehouse.getPhone())
                        .address(requestWarehouse.getAddress())
                        .contact(requestWarehouse.getContact().toUpperCase())
                        .reference(requestWarehouse.getReference().toUpperCase())
                        .name(requestWarehouse.getName().toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .status(true)
                        .user(user).userId(user.getId())
                        .build());
                iAudit.save("ADD_WAREHOUSE","ALMACEN "+newWarehouse.getName()+" CREADO.",newWarehouse.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String warehouseName, String tokenUser) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(),warehouseName.toUpperCase());
            }
            if(warehouse==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }
            try {
                warehouse.setStatus(false);
                warehouse.setUpdateDate(OffsetDateTime.now());
                warehouse.setUser(user);
                warehouse.setUserId(user.getId());
                iAudit.save("DELETE_WAREHOUSE","ALMACEN "+warehouse.getName()+" DESACTIVADO.",warehouse.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String warehouseName, String tokenUser) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(),warehouseName.toUpperCase());
            }
            if(warehouse==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }
            try {
                warehouse.setStatus(false);
                warehouse.setUpdateDate(OffsetDateTime.now());
                warehouse.setUser(user);
                warehouse.setUserId(user.getId());
                iAudit.save("ACTIVATE_WAREHOUSE","ALMACEN "+warehouse.getName()+" ACTIVADO.",warehouse.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<WarehouseDTO>> list(
            String user,
            List<String> names,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Warehouse> warehousePage;
            UUID clientId;
            List<String> namesUppercase;

            if(names != null && !names.isEmpty()){
                namesUppercase = names.stream().map(String::toUpperCase).toList();
            }else{
                namesUppercase = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                warehousePage = warehouseRepositoryCustom.searchForWarehouse(
                        clientId,
                        namesUppercase,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize, true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (warehousePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<WarehouseDTO> warehouseDTOs = warehousePage.getContent().stream().map(warehouse -> WarehouseDTO.builder()
                    .id(warehouse.getId())
                    .status(warehouse.getStatus())
                    .user(warehouse.getUser().getUsername())
                    .name(warehouse.getName())
                    .contact(warehouse.getContact())
                    .phone(warehouse.getPhone())
                    .address(warehouse.getAddress())
                    .reference(warehouse.getReference())
                    .registrationDate(warehouse.getRegistrationDate())
                    .updateDate(warehouse.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(warehouseDTOs, warehousePage.getPageable(), warehousePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<WarehouseDTO>> listWarehouse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Warehouse> warehouses;
            UUID clientId;
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
                    .id(warehouse.getId())
                    .status(warehouse.getStatus())
                    .user(warehouse.getUser().getUsername())
                    .name(warehouse.getName())
                    .contact(warehouse.getContact())
                    .phone(warehouse.getPhone())
                    .address(warehouse.getAddress())
                    .reference(warehouse.getReference())
                    .registrationDate(warehouse.getRegistrationDate())
                    .updateDate(warehouse.getUpdateDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<WarehouseDTO>> listWarehouseFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Warehouse> warehouses;
            UUID clientId;
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
                    .id(warehouse.getId())
                    .status(warehouse.getStatus())
                    .user(warehouse.getUser().getUsername())
                    .name(warehouse.getName())
                    .contact(warehouse.getContact())
                    .phone(warehouse.getPhone())
                    .address(warehouse.getAddress())
                    .reference(warehouse.getReference())
                    .registrationDate(warehouse.getRegistrationDate())
                    .updateDate(warehouse.getUpdateDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<WarehouseDTO>> listFilters(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Warehouse> warehouses;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                warehouses = warehouseRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (warehouses.isEmpty()){
                return Collections.emptyList();
            }

            return warehouses.stream().map(warehouse -> WarehouseDTO.builder()
                    .id(warehouse.getId())
                    .status(warehouse.getStatus())
                    .user(warehouse.getUser().getUsername())
                    .name(warehouse.getName())
                    .contact(warehouse.getContact())
                    .phone(warehouse.getPhone())
                    .address(warehouse.getAddress())
                    .reference(warehouse.getReference())
                    .registrationDate(warehouse.getRegistrationDate())
                    .updateDate(warehouse.getUpdateDate())
                    .build()).toList();
        });
    }
}
