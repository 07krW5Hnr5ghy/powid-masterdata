package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.Store;
import com.proyect.masterdata.dto.ClientChannelDTO;
import com.proyect.masterdata.dto.request.RequestClientChannel;
import com.proyect.masterdata.dto.request.RequestClientChannelSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.StoreMapper;
import com.proyect.masterdata.repository.StoreRepository;
import com.proyect.masterdata.repository.StoreRepositoryCustom;
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

    @Override
    public ResponseSuccess save(String ruc, RequestClientChannelSave requestClientChannelSave, String user)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsStore;
        Client client;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            existsStore = storeRepository
                    .existsByName(requestClientChannelSave.getName().toUpperCase());
            client = clientRepository.findByRuc(ruc);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario incorrecto");
        }
        if (existsStore) {
            throw new BadRequestExceptions("El canal ya existe");
        }
        if (client == null) {
            throw new BadRequestExceptions("cliente no existe");
        }

        try {
            storeRepository.save(Store.builder()
                    .name(requestClientChannelSave.getName().toUpperCase())
                    .url(requestClientChannelSave.getUrl())
                    .idClient(client.getIdClient())
                    .dateRegistration(new Date(System.currentTimeMillis()))
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
    public ResponseSuccess saveAll(String ruc, List<RequestClientChannelSave> clientChannelList, String user)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        List<Store> stores;
        Client client;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            stores = storeRepository.findByNameIn(clientChannelList.stream()
                    .map(clientChannel -> clientChannel.getName().toUpperCase()).collect(Collectors.toList()));
            client = clientRepository.findByRuc(ruc);
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (!stores.isEmpty()) {
            throw new BadRequestExceptions("El canal ya existe");
        }
        if (client == null) {
            throw new BadRequestExceptions("cliente no existe");
        }
        List<Store> clientChannelSaveList = clientChannelList.stream()
                .map(clientChannel -> Store.builder()
                        .name(clientChannel.getName().toUpperCase())
                        .url(clientChannel.getUrl())
                        .idClient(client.getIdClient())
                        .status(true)
                        .user(user.toUpperCase())
                        .build())
                .toList();
        try {
            clientChannelRepository.saveAll(clientChannelSaveList);
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
    public ClientChannelDTO update(RequestClientChannel requestClientChannel)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        Store clientChannel;
        try {
            existsUser = userRepository.existsByUsername(requestClientChannel.getUser().toUpperCase());
            clientChannel = clientChannelRepository.findById(requestClientChannel.getCode()).orElse(null);
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (clientChannel == null) {
            throw new BadRequestExceptions("Canal no existe");
        }
        clientChannel.setName(requestClientChannel.getName().toUpperCase());
        clientChannel.setUrl(requestClientChannel.getUrl());
        clientChannel.setStatus(requestClientChannel.isStatus());
        clientChannel.setUser(requestClientChannel.getUser().toUpperCase());
        clientChannel.setDateRegistration(new Date(System.currentTimeMillis()));
        try {
            return clientChannelMapper.clientChannelToClientChannelDTO(clientChannelRepository.save(clientChannel));
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        Store clientChannel;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            clientChannel = clientChannelRepository.findById(code).orElse(null);
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (clientChannel == null) {
            throw new BadRequestExceptions("Canal no existe");
        }
        clientChannel.setStatus(false);
        clientChannel.setDateRegistration(new Date(System.currentTimeMillis()));
        try {
            clientChannelRepository.save(clientChannel);
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
    public List<ClientChannelDTO> listClientChannel() {
        List<Store> clientChannels;
        try {
            clientChannels = clientChannelRepository.findAllByStatusTrue();
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (clientChannels.isEmpty()) {
            return Collections.emptyList();
        }
        return clientChannelMapper.listClientChannelToListClientChannelDTO(clientChannels);
    }

    @Override
    public Page<ClientChannelDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        Page<Store> clientChannelPage;
        try {
            clientChannelPage = clientChannelRepositoryCustom.searchForClientChannel(name, user, sort, sortColumn,
                    pageNumber, pageSize, true);
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (clientChannelPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(
                clientChannelMapper.listClientChannelToListClientChannelDTO(clientChannelPage.getContent()),
                clientChannelPage.getPageable(), clientChannelPage.getTotalElements());
    }

    @Override
    public Page<ClientChannelDTO> listStatusFalse(String name, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<Store> clientChannelPage;
        try {
            clientChannelPage = clientChannelRepositoryCustom.searchForClientChannel(name, user, sort, sortColumn,
                    pageNumber, pageSize, false);
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (clientChannelPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(
                clientChannelMapper.listClientChannelToListClientChannelDTO(clientChannelPage.getContent()),
                clientChannelPage.getPageable(), clientChannelPage.getTotalElements());
    }

    @Override
    public ClientChannelDTO findByCode(Long code) throws BadRequestExceptions {
        try {
            return clientChannelMapper
                    .clientChannelToClientChannelDTO(clientChannelRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
