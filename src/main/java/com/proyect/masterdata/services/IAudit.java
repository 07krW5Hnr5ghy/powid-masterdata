package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.AuditDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.concurrent.CompletableFuture;

public interface IAudit {
    CompletableFuture<ResponseSuccess> save(String eventName, String detail, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<AuditDTO>> list(String username,String eventName,String clientRuc,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions,InternalErrorExceptions;
}
