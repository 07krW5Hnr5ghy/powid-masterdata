package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CountryDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ICountry {
    CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<CountryDTO>> listCountry(String name, String sort, String sortColumn, Integer pageNumber,
                                 Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<CountryDTO>> listFilter() throws BadRequestExceptions,InternalErrorExceptions;
}
