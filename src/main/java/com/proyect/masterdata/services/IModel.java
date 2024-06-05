package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ModelDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IModel {
        ResponseSuccess save(String name, String brand, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(String name, String brand, String tokenUser)
                throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<ModelDTO>> list(String name, String brand, String tokenUser, String sort, String columnSort,
                        Integer pageNumber,
                        Integer pageSize);
        CompletableFuture<Page<ModelDTO>> listStatusFalse(String name, String brand, String tokenUser, String sort, String columnSort,
                        Integer pageNumber,
                        Integer pageSize);
        CompletableFuture<List<ModelDTO>> listModels(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ModelDTO>> listModelsFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ModelDTO>> listModelBrand(String user,String brand) throws BadRequestExceptions,InternalErrorExceptions;
}
