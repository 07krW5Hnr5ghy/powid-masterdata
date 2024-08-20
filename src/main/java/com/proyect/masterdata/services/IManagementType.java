package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ManagementTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IManagementType {
    CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String name,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(String name,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<ManagementTypeDTO>> listPagination(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<Page<ManagementTypeDTO>> listFalse(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<List<String>> list() throws InternalErrorExceptions,BadRequestExceptions;
}
