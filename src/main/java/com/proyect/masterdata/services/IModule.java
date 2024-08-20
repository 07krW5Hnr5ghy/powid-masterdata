package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.dto.request.RequestModule;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IModule {
        ResponseSuccess save(String name, double price, String tokenUser)
                        throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(String name, double price, String tokenUser)
                throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ModuleDTO> update(RequestModule requestModule, String tokenUser)
                        throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<List<ModuleDTO>> listModule() throws BadRequestExceptions;
        CompletableFuture<Page<ModuleDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize)
                        throws BadRequestExceptions;
        CompletableFuture<Page<ModuleDTO>> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize) throws BadRequestExceptions;
}
