package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.Store;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.services.IAudit;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.StoreType;
import com.proyect.masterdata.dto.StoreTypeDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.StoreTypeMapper;
import com.proyect.masterdata.repository.StoreTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IStoreType;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class StoreTypeImpl implements IStoreType {

    private final StoreTypeRepository storeTypeRepository;
    private final UserRepository userRepository;
    private final StoreTypeMapper storeTypeMapper;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsStoreType;
        User user;

        try {
            existsStoreType = storeTypeRepository.existsByName(name.toUpperCase());
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user==null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (existsStoreType) {
            throw new BadRequestExceptions(Constants.ErrorStoreTypeExists);
        }

        try {
            StoreType newStoreType = storeTypeRepository.save(StoreType.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .user(user).userId(user.getId())
                    .build());
            iAudit.save("ADD_STORE_TYPE","TIPO DE TIENDA "+newStoreType.getName()+" CREADO.",newStoreType.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            boolean existsStoreType;
            User user;

            try {
                existsStoreType = storeTypeRepository.existsByName(name.toUpperCase());
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (existsStoreType) {
                throw new BadRequestExceptions(Constants.ErrorStoreTypeExists);
            }

            try {
                StoreType newStoreType = storeTypeRepository.save(StoreType.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .user(user).userId(user.getId())
                        .build());
                iAudit.save("ADD_STORE_TYPE","TIPO DE TIENDA "+newStoreType.getName()+" CREADO.",newStoreType.getName(),user.getUsername());
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
            StoreType storeType;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                storeType = storeTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(storeType==null){
                throw new BadRequestExceptions(Constants.ErrorStoreType);
            }
            try {
                storeType.setStatus(false);
                storeType.setUpdateDate(OffsetDateTime.now());
                storeType.setUser(user);
                storeType.setUserId(user.getId());
                iAudit.save("DELETE_STORE_TYPE","TIPO DE TIENDA "+storeType.getName()+" DESACTIVADO.",storeType.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.ErrorStoreType)
                        .code(200)
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
            StoreType storeType;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                storeType = storeTypeRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(storeType==null){
                throw new BadRequestExceptions(Constants.ErrorStoreType);
            }
            try {
                storeType.setStatus(true);
                storeType.setUpdateDate(OffsetDateTime.now());
                storeType.setUser(user);
                storeType.setUserId(user.getId());
                iAudit.save("ACTIVATE_STORE_TYPE","TIPO DE TIENDA "+storeType.getName()+" ACTIVADO.",storeType.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.ErrorStoreType)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<StoreTypeDTO>> listStoreType() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StoreType> storeTypes;

            try {
                storeTypes = storeTypeRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (storeTypes.isEmpty()) {
                return Collections.emptyList();
            }

            return storeTypeMapper.listStoreTypeToListStoreTypeDTO(storeTypes);
        });
    }

}
