package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SaleChannelDTO;
import com.proyect.masterdata.dto.request.RequestSaleChannel;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ISaleChannel {
    ResponseSuccess save(String name,String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(String name, String user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(String name,String user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<SaleChannelDTO>> listSaleChannel() throws BadRequestExceptions;
    CompletableFuture<Page<SaleChannelDTO>> list(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<Page<SaleChannelDTO>> listStatusFalse(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> activate(String name,String user) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<SaleChannelDTO>> listFilter() throws BadRequestExceptions;
}
