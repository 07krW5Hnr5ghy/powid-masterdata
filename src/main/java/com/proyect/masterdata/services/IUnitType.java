package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.UnitTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IUnitType {
    ResponseSuccess save(String name,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<UnitTypeDTO>> listUnitType() throws BadRequestExceptions;
}
