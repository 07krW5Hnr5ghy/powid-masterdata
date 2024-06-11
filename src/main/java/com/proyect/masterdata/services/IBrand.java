package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.dto.BrandDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.springframework.data.domain.Page;

public interface IBrand {
        ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<BrandDTO>> listPagination(String name, String tokenUser, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber,
                                                         Integer pageSize)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<BrandDTO>> listStatusFalse(String name, String tokenUser,Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn,
                        Integer pageNumber,
                        Integer pageSize)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> activate(String name,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
        CompletableFuture<List<BrandDTO>> listBrands(String user) throws BadRequestExceptions, InternalErrorExceptions;
        CompletableFuture<List<BrandDTO>> listBrandsFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
