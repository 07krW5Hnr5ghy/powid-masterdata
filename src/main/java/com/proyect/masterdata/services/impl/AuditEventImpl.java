package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.AuditEvent;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.AuditEventDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.AuditEventRepository;
import com.proyect.masterdata.repository.AuditEventRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAuditEvent;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class AuditEventImpl implements IAuditEvent {
    private final AuditEventRepository auditEventRepository;
    private final UserRepository userRepository;
    private final AuditEventRepositoryCustom auditEventRepositoryCustom;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        AuditEvent auditEvent;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            auditEvent = auditEventRepository.findByName(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(user==null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(auditEvent!=null){
            throw new BadRequestExceptions(Constants.ErrorAuditEventExists);
        }
        try {
            auditEventRepository.save(AuditEvent.builder()
                            .status(true)
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .name(name.toUpperCase())
                            .user(user)
                            .userId(user.getId())
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
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
            AuditEvent auditEvent;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                auditEvent = auditEventRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(auditEvent!=null){
                throw new BadRequestExceptions(Constants.ErrorAuditEventExists);
            }
            try {
                auditEventRepository.save(AuditEvent.builder()
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                        .name(name.toUpperCase())
                                .user(user)
                                .userId(user.getId())
                        .build());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
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
            AuditEvent auditEvent;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                auditEvent = auditEventRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(auditEvent==null){
                throw new BadRequestExceptions(Constants.ErrorAuditEvent);
            }
            try {
                auditEvent.setStatus(false);
                auditEvent.setUpdateDate(OffsetDateTime.now());
                auditEvent.setUser(user);
                auditEvent.setUserId(user.getId());
                auditEventRepository.save(auditEvent);
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
    public CompletableFuture<List<String>> list() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<AuditEvent> auditEvents;
            try {
                auditEvents = auditEventRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(auditEvents.isEmpty()){
                return Collections.emptyList();
            }
            return auditEvents.stream().map(AuditEvent::getName).toList();
        });
    }

    @Override
    public CompletableFuture<Page<AuditEventDTO>> listPagination(String name, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<AuditEvent> auditEventPage;
            try {
                auditEventPage = auditEventRepositoryCustom.searchForAuditEvent(name,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate,sort,sortColumn,pageNumber,pageSize,true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(auditEventPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<AuditEventDTO> auditEventDTOS = auditEventPage.getContent().stream().map(auditEvent -> AuditEventDTO.builder()
                    .name(auditEvent.getName())
                    .id(auditEvent.getId())
                    .registrationDate(auditEvent.getRegistrationDate())
                    .updateDate(auditEvent.getUpdateDate())
                    .user(auditEvent.getUser().getUsername())
                    .build()).toList();
            return new PageImpl<>(auditEventDTOS,auditEventPage.getPageable(),auditEventPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<AuditEventDTO>> listFalse(String name, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<AuditEvent> auditEventPage;
            try {
                auditEventPage = auditEventRepositoryCustom.searchForAuditEvent(name,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate,sort,sortColumn,pageNumber,pageSize,false);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(auditEventPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<AuditEventDTO> auditEventDTOS = auditEventPage.getContent().stream().map(auditEvent -> AuditEventDTO.builder()
                    .name(auditEvent.getName())
                    .id(auditEvent.getId())
                    .registrationDate(auditEvent.getRegistrationDate())
                    .updateDate(auditEvent.getUpdateDate())
                    .user(auditEvent.getUser().getUsername())
                    .build()).toList();
            return new PageImpl<>(auditEventDTOS,auditEventPage.getPageable(),auditEventPage.getTotalElements());
        });
    }
}
