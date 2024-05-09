package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.StoreDTO;
import com.proyect.masterdata.dto.request.RequestStore;
import com.proyect.masterdata.dto.request.RequestStoreSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IStore {
    ResponseSuccess save(RequestStoreSave requestStoreSave, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestStoreSave requestStoreSave, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<StoreDTO> update(RequestStore requestStore) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<StoreDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize)
            throws BadRequestExceptions;
    CompletableFuture<Page<StoreDTO>> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<StoreDTO>> listStore(String user) throws BadRequestExceptions;
}
