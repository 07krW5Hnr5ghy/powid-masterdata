package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Store;
import com.proyect.masterdata.domain.StoreType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.StoreDTO;
import com.proyect.masterdata.dto.request.RequestStore;
import com.proyect.masterdata.dto.request.RequestStoreSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.StoreMapper;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IStore;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class StoreImpl implements IStore {

    private final StoreRepository storeRepository;
    private final StoreRepositoryCustom storeRepositoryCustom;
    private final StoreMapper storeMapper;
    private final UserRepository userRepository;
    private final ClientRepository clientRepository;
    private final StoreTypeRepository storeTypeRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(RequestStoreSave requestStoreSave, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        boolean existsStore;
        StoreType storeType;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            existsStore = storeRepository
                    .existsByName(requestStoreSave.getName().toUpperCase());
            storeType = storeTypeRepository.findByNameAndStatusTrue(requestStoreSave.getStoreType().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (existsStore) {
            throw new BadRequestExceptions(Constants.ErrorStoreExist);
        }

        if (storeType == null) {
            throw new BadRequestExceptions(Constants.ErrorStoreType);
        }

        try {

            Store newStore = storeRepository.save(Store.builder()
                    .name(requestStoreSave.getName().toUpperCase())
                    .url(requestStoreSave.getUrl())
                    .clientId(user.getClientId())
                    .client(user.getClient())
                    .storeType(storeType)
                    .storeTypeId(storeType.getId())
                    .registrationDate(OffsetDateTime.now())
                    .status(true)
                    .user(user).userId(user.getId())
                    .build());
            iAudit.save("ADD_STORE","TIENDA "+newStore.getName()+" AGREGADA.",newStore.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();

        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(RequestStoreSave requestStoreSave, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            boolean existsStore;
            StoreType storeType;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                existsStore = storeRepository
                        .existsByName(requestStoreSave.getName().toUpperCase());
                storeType = storeTypeRepository.findByNameAndStatusTrue(requestStoreSave.getStoreType().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (existsStore) {
                throw new BadRequestExceptions(Constants.ErrorStoreExist);
            }

            if (storeType == null) {
                throw new BadRequestExceptions(Constants.ErrorStoreType);
            }

            try {

                Store newStore = storeRepository.save(Store.builder()
                        .name(requestStoreSave.getName().toUpperCase())
                        .url(requestStoreSave.getUrl())
                        .clientId(user.getClientId())
                        .client(user.getClient())
                        .storeType(storeType)
                        .storeTypeId(storeType.getId())
                        .registrationDate(OffsetDateTime.now())
                        .status(true)
                        .user(user).userId(user.getId())
                        .build());
                iAudit.save("ADD_STORE","TIENDA "+newStore.getName()+" AGREGADA.",newStore.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();

            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<StoreDTO> update(RequestStore requestStore)
            throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Store store;

            try {
                user = userRepository.findByUsernameAndStatusTrue(requestStore.getTokenUser().toUpperCase());
                store = storeRepository.findByNameAndStatusTrue(requestStore.getName().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (store == null) {
                throw new BadRequestExceptions(Constants.ErrorStore);
            }

            try {

                store.setUrl(requestStore.getUrl());
                store.setUser(user);
                store.setUserId(user.getId());
                store.setUpdateDate(OffsetDateTime.now());
                iAudit.save("UPDATE_STORE","TIENDA "+store.getName()+" ACTUALIZADA.",store.getName(),user.getUsername());
                return StoreDTO.builder()
                        .name(store.getName())
                        .url(store.getUrl())
                        .client(store.getClient().getBusiness())
                        .storeType(store.getStoreType().getName())
                        .build();

            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Store store;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                store = storeRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (store == null) {
                throw new BadRequestExceptions(Constants.ErrorStore);
            }

            try {
                store.setStatus(false);
                store.setRegistrationDate(OffsetDateTime.now());
                store.setUser(user);
                store.setUserId(user.getId());
                storeRepository.save(store);
                iAudit.save("DELETE_STORE","TIENDA "+store.getName()+" DESACTIVADA.",store.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Store store;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                store = storeRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (store == null) {
                throw new BadRequestExceptions(Constants.ErrorStore);
            }

            try {
                store.setStatus(true);
                store.setRegistrationDate(OffsetDateTime.now());
                store.setUser(user);
                store.setUserId(user.getId());
                storeRepository.save(store);
                iAudit.save("ACTIVATE_STORE","TIENDA "+store.getName()+" ACTIVADA.",store.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<StoreDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Store> storePage;

            try {

                storePage = storeRepositoryCustom.searchForStore(name, user, sort, sortColumn,
                        pageNumber, pageSize, true);

            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (storePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<StoreDTO> storeDTOs = storePage.getContent().stream().map(store -> StoreDTO.builder()
                    .name(store.getName())
                    .url(store.getUrl())
                    .client(store.getClient().getBusiness())
                    .storeType(store.getStoreType().getName())
                    .user(store.getUser().getUsername())
                    .build()).toList();

            return new PageImpl<>(
                    storeDTOs,
                    storePage.getPageable(), storePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<StoreDTO>> listStatusFalse(String name, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Store> storePage;

            try {
                storePage = storeRepositoryCustom.searchForStore(name, user, sort, sortColumn,
                        pageNumber, pageSize, false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (storePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<StoreDTO> storeDTOs = storePage.getContent().stream().map(store -> StoreDTO.builder()
                    .name(store.getName())
                    .url(store.getUrl())
                    .client(store.getClient().getBusiness())
                    .storeType(store.getStoreType().getName())
                    .user(store.getUser().getUsername())
                    .build()).toList();

            return new PageImpl<>(
                    storeDTOs,
                    storePage.getPageable(), storePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<StoreDTO>> listStore(String user) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Store> stores;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                stores = storeRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(stores.isEmpty()){
                return Collections.emptyList();
            }
            return stores.stream().map(store -> StoreDTO.builder()
                    .name(store.getName())
                    .url(store.getUrl())
                    .client(store.getClient().getBusiness())
                    .storeType(store.getStoreType().getName())
                    .user(store.getUser().getUsername())
                    .build()).toList();
        });
    }
}
