package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.AuditEventDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IAuditEvent {
    ResponseSuccess save(String name,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(String name,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<String>> list() throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<AuditEventDTO>> listPagination(String name, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber,
                                                          Integer pageSize) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<AuditEventDTO>> listFalse(String name, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber,
                                                          Integer pageSize) throws BadRequestExceptions,InternalErrorExceptions;
}
