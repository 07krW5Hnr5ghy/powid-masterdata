package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.EntryChannelDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.concurrent.CompletableFuture;

public interface IEntryChannel {
    CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<EntryChannelDTO>> listEntryChannel(String name, String sort, String sortColumn, Integer pageNumber,
                                                              Integer pageSize) throws BadRequestExceptions;
}
