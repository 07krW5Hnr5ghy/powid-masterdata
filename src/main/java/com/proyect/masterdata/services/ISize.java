package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ISize {
        ResponseSuccess save(String name, String sizeType, String tokenUser)
                        throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(String name, String sizeType, String tokenUser)
                throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<List<SizeDTO>> listSize() throws BadRequestExceptions;
        CompletableFuture<Page<SizeDTO>> list(String name, String user, String sort,
                        String sortColumn,
                        Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<Page<SizeDTO>> listStatusFalse(String name, String user, String sort,
                        String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<List<SizeDTO>> findAllSizeTypeName(String nameSizeType) throws BadRequestExceptions;
        CompletableFuture<List<SizeDTO>> listFilter() throws BadRequestExceptions;
}
