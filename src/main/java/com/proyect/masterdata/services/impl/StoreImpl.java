package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.Store;
import com.proyect.masterdata.domain.StoreType;
import com.proyect.masterdata.dto.StoreDTO;
import com.proyect.masterdata.dto.request.RequestStore;
import com.proyect.masterdata.dto.request.RequestStoreSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.StoreMapper;
import com.proyect.masterdata.repository.StoreRepository;
import com.proyect.masterdata.repository.StoreRepositoryCustom;
import com.proyect.masterdata.repository.StoreTypeRepository;
import com.proyect.masterdata.repository.ClientRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IStore;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

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

    @Override
    public ResponseSuccess save(RequestStoreSave requestStoreSave, String user)
            throws BadRequestExceptions, InternalErrorExceptions {

        boolean existsUser;
        boolean existsStore;
        Client client;
        StoreType storeType;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(user.toUpperCase());
            existsStore = storeRepository
                    .existsByName(requestStoreSave.getName().toUpperCase());
            client = clientRepository.findByRucAndStatusTrue(requestStoreSave.getClientRuc());
            storeType = storeTypeRepository.findByNameAndStatusTrue(requestStoreSave.getStoreType().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (existsStore) {
            throw new BadRequestExceptions(Constants.ErrorStoreExist);
        }

        if (client == null) {
            throw new BadRequestExceptions(Constants.ErrorClient);
        }

        if (storeType == null) {
            throw new BadRequestExceptions(Constants.ErrorStoreType);
        }

        try {

            storeRepository.save(Store.builder()
                    .name(requestStoreSave.getName().toUpperCase())
                    .url(requestStoreSave.getUrl())
                    .clientId(client.getId())
                    .client(client)
                    .storeType(storeType)
                    .storeTypeId(storeType.getId())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(user.toUpperCase())
                    .build());

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
    public StoreDTO update(RequestStore requestStore)
            throws BadRequestExceptions, InternalErrorExceptions {

        boolean existsUser;
        Store store;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(requestStore.getTokenUser().toUpperCase());
            store = storeRepository.findByNameAndStatusTrue(requestStore.getName().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (store == null) {
            throw new BadRequestExceptions(Constants.ErrorStore);
        }

        store.setUrl(requestStore.getUrl());
        store.setTokenUser(requestStore.getTokenUser().toUpperCase());
        store.setUpdateDate(new Date(System.currentTimeMillis()));

        try {

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
    }

    @Override
    public ResponseDelete delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {

        boolean existsUser;
        Store store;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(user.toUpperCase());
            store = storeRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (store == null) {
            throw new BadRequestExceptions(Constants.ErrorStore);
        }

        store.setStatus(false);
        store.setRegistrationDate(new Date(System.currentTimeMillis()));
        store.setTokenUser(user.toUpperCase());

        try {

            storeRepository.save(store);

            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<StoreDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {

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
                .user(store.getTokenUser())
                .build()).toList();

        return new PageImpl<>(
                storeDTOs,
                storePage.getPageable(), storePage.getTotalElements());
    }

    @Override
    public Page<StoreDTO> listStatusFalse(String name, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {

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
                .user(store.getTokenUser())
                .build()).toList();

        return new PageImpl<>(
                storeDTOs,
                storePage.getPageable(), storePage.getTotalElements());
    }

}
