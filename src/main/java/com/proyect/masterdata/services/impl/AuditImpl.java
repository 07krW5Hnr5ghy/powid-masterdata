package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Audit;
import com.proyect.masterdata.domain.AuditEvent;
import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.AuditDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class AuditImpl implements IAudit {
    private final UserRepository userRepository;
    private final AuditRepository auditRepository;
    private final AuditEventRepository auditEventRepository;
    private final AuditRepositoryCustom auditRepositoryCustom;
    private final ClientRepository clientRepository;
    @Override
    public CompletableFuture<ResponseSuccess> save(String eventName, String detail, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            AuditEvent auditEvent;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                auditEvent = auditEventRepository.findByNameAndStatusTrue(eventName.toUpperCase());
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
                auditRepository.save(Audit.builder()
                                .auditEvent(auditEvent)
                                .auditEventId(auditEvent.getId())
                                .client(user.getClient())
                                .clientId(user.getClientId())
                                .detail(detail.toUpperCase())
                                .registrationDate(new Date(System.currentTimeMillis()))
                                .tokenUser(user.getUsername())
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
    public CompletableFuture<Page<AuditDTO>> list(String username, String eventName, String clientRuc, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Audit> auditPage;
            Long userId;
            Long clientId;
            Long auditEventId;
            if(username != null){
                userId = userRepository.findByUsernameAndStatusTrue(username.toUpperCase()).getId();
            }else {
                userId = null;
            }
            if(eventName!=null){
                auditEventId = auditEventRepository.findByNameAndStatusTrue(eventName.toUpperCase()).getId();
            }else{
                auditEventId = null;
            }
            if(clientRuc != null){
                clientId = clientRepository.findByRuc(clientRuc).getId();
            }else {
                clientId = null;
            }
            try {
                auditPage = auditRepositoryCustom.searchForAudit(userId,auditEventId,clientId,sort,sortColumn,pageNumber,pageSize);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(auditPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<AuditDTO> auditDTOS = auditPage.stream().map(audit -> AuditDTO.builder()
                    .userName(audit.getUser().getName())
                    .clientName(audit.getClient().getName())
                    .eventName(audit.getAuditEvent().getName())
                    .registrationDate(audit.getRegistrationDate())
                    .detail(audit.getDetail())
                    .build()).toList();
            return new PageImpl<>(auditDTOS,auditPage.getPageable(),auditPage.getTotalElements());
        });
    }
}
