package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.SupplierType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.SupplierTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ISupplierType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class SupplierTypeImpl implements ISupplierType {
    private final UserRepository userRepository;
    private final SupplierTypeRepository supplierTypeRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        SupplierType supplierType;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierType = supplierTypeRepository.findByNameAndClientIdAndStatusTrue(name.toUpperCase(),user.getClientId());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(supplierType != null){
            throw new BadRequestExceptions(Constants.ErrorSupplierTypeExists);
        }
        try {
            SupplierType newSupplierType = supplierTypeRepository.save(SupplierType.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                            .user(user)
                            .userId(user.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                    .build());
            iAudit.save("ADD_SUPPLIER_TYPE","TIPO DE PROVEEDOR "+newSupplierType.getName()+" CREADO.",newSupplierType.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .message(Constants.register)
                    .code(200)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierType supplierType;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierType = supplierTypeRepository.findByNameAndClientIdAndStatusTrue(name.toUpperCase(),user.getClientId());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(supplierType != null){
                throw new BadRequestExceptions(Constants.ErrorSupplierTypeExists);
            }
            try {
                SupplierType newSupplierType = supplierTypeRepository.save(SupplierType.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .user(user)
                        .userId(user.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .build());
                iAudit.save("ADD_SUPPLIER_TYPE","TIPO DE PROVEEDOR "+newSupplierType.getName()+" CREADO.",newSupplierType.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierType supplierType;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierType = supplierTypeRepository.findByNameAndClientIdAndStatusTrue(name.toUpperCase(),user.getClientId());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(supplierType == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierType);
            }
            try {
                supplierType.setStatus(false);
                supplierType.setUpdateDate(OffsetDateTime.now());
                supplierType.setUser(user);
                supplierType.setUserId(user.getId());
                iAudit.save("DELETE_SUPPLIER_TYPE","TIPO DE PROVEEDOR "+supplierType.getName()+" DESACTIVADO.",supplierType.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierType supplierType;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());

            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                supplierType = supplierTypeRepository.findByNameAndClientIdAndStatusFalse(name.toUpperCase(),user.getClientId());
            }
            if(supplierType == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierType);
            }
            try {
                supplierType.setStatus(true);
                supplierType.setUpdateDate(OffsetDateTime.now());
                supplierType.setUser(user);
                supplierType.setUserId(user.getId());
                iAudit.save("ACTIVATE_SUPPLIER_TYPE","TIPO DE PROVEEDOR "+supplierType.getName()+" ACTIVADO.",supplierType.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<String>> listSupplierType() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SupplierType> supplierTypes;
            try {
                supplierTypes = supplierTypeRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(supplierTypes.isEmpty()){
                return Collections.emptyList();
            }
            return supplierTypes.stream().map(SupplierType::getName).toList();
        });
    }

    @Override
    public CompletableFuture<List<String>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SupplierType> supplierTypes;
            try {
                supplierTypes = supplierTypeRepository.findAll();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(supplierTypes.isEmpty()){
                return Collections.emptyList();
            }
            return supplierTypes.stream().map(SupplierType::getName).toList();
        });
    }
}
