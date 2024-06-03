package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.AuditEvent;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.AuditEventRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAuditEvent;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

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
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .name(name.toUpperCase())
                            .tokenUser(user.getUsername())
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
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .name(name.toUpperCase())
                        .tokenUser(user.getUsername())
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
                auditEvent.setUpdateDate(new Date(System.currentTimeMillis()));
                auditEvent.setTokenUser(user.getUsername());
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
}
