package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.repository.EntryChannelRepositoryCustom;
import com.proyect.masterdata.services.IAudit;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.EntryChannel;
import com.proyect.masterdata.dto.EntryChannelDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.EntryChannelMapper;
import com.proyect.masterdata.repository.EntryChannelRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IEntryChannel;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class EntryChannelImpl implements IEntryChannel {

    private final UserRepository userRepository;
    private final EntryChannelRepository entryChannelRepository;
    private final EntryChannelMapper entryChannelMapper;
    private final EntryChannelRepositoryCustom entryChannelRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            boolean existsEntryChannel;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                existsEntryChannel = entryChannelRepository.existsByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (existsEntryChannel) {
                throw new BadRequestExceptions(Constants.ErrorEntryChannelExists);
            }

            try {
                EntryChannel newEntryChannel = entryChannelRepository.save(EntryChannel.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .user(user)
                                .userId(user.getId())
                        .build());
                iAudit.save("ADD_ENTRY_CHANNEL","CANAL DE ENTRADA "+newEntryChannel.getName()+" CREADO.",newEntryChannel.getName(),user.getUsername());
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
    public CompletableFuture<Page<EntryChannelDTO>> listEntryChannel(
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<EntryChannel> entryChannelPage;

            try {
                entryChannelPage = entryChannelRepositoryCustom.searchEntryChannel(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (entryChannelPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<EntryChannelDTO> entryChannelDTOS = entryChannelPage.getContent().stream().map(entryChannel -> EntryChannelDTO.builder()
                    .id(entryChannel.getId())
                    .updateDate(entryChannel.getUpdateDate())
                    .registrationDate(entryChannel.getRegistrationDate())
                    .name(entryChannel.getName())
                    .status(entryChannel.getStatus())
                    .user(entryChannel.getUser().getUsername())
                    .build()).toList();
            return new PageImpl<>(entryChannelDTOS,entryChannelPage.getPageable(),entryChannelPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<EntryChannelDTO>> listFalse(String name, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<EntryChannel> entryChannelPage;

            try {
                entryChannelPage = entryChannelRepositoryCustom.searchEntryChannel(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (entryChannelPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<EntryChannelDTO> entryChannelDTOS = entryChannelPage.getContent().stream().map(entryChannel -> EntryChannelDTO.builder()
                    .id(entryChannel.getId())
                    .updateDate(entryChannel.getUpdateDate())
                    .registrationDate(entryChannel.getRegistrationDate())
                    .name(entryChannel.getName())
                    .status(entryChannel.getStatus())
                    .user(entryChannel.getUser().getUsername())
                    .build()).toList();
            return new PageImpl<>(entryChannelDTOS,entryChannelPage.getPageable(),entryChannelPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<EntryChannelDTO>> list() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<EntryChannel> entryChannelList;
            try {
                entryChannelList = entryChannelRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(entryChannelList.isEmpty()){
                return Collections.emptyList();
            }
            return entryChannelList.stream().map(entryChannel -> EntryChannelDTO.builder()
                    .id(entryChannel.getId())
                    .updateDate(entryChannel.getUpdateDate())
                    .registrationDate(entryChannel.getRegistrationDate())
                    .name(entryChannel.getName())
                    .status(entryChannel.getStatus())
                    .user(entryChannel.getUser().getUsername())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            EntryChannel entryChannel;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                entryChannel = entryChannelRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(entryChannel==null){
                throw new BadRequestExceptions(Constants.ErrorEntryChannel);
            }
            try {
                entryChannel.setStatus(false);
                entryChannel.setUpdateDate(OffsetDateTime.now());
                entryChannel.setUser(user);
                entryChannel.setUserId(user.getId());
                entryChannelRepository.save(entryChannel);
                iAudit.save("DELETE_ENTRY_CHANNEL","CANAL DE ENTRADA "+entryChannel.getName()+" DESACTIVADO.",entryChannel.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            EntryChannel entryChannel;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                entryChannel = entryChannelRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(entryChannel==null){
                throw new BadRequestExceptions(Constants.ErrorEntryChannel);
            }
            try {
                entryChannel.setStatus(true);
                entryChannel.setUpdateDate(OffsetDateTime.now());
                entryChannel.setUser(user);
                entryChannel.setUserId(user.getId());
                entryChannelRepository.save(entryChannel);
                iAudit.save("ACTIVATE_ENTRY_CHANNEL","CANAL DE ENTRADA "+entryChannel.getName()+" ACTIVADO.",entryChannel.getName(),user.getUsername());
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
}
