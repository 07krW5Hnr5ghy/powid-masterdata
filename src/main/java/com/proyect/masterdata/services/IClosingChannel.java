package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.dto.ClosingChannelDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

public interface IClosingChannel {
    CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<ClosingChannelDTO>> listClosingChannel(String name, String sort, String sortColumn, Integer pageNumber,
                                                                  Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<ClosingChannelDTO>> list() throws BadRequestExceptions;
}
