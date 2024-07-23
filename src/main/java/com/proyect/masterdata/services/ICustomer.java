package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Customer;
import com.proyect.masterdata.dto.CustomerDTO;
import com.proyect.masterdata.dto.request.RequestCustomer;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ICustomer {
    ResponseSuccess save(RequestCustomer requestCustomer) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestCustomer requestCustomer) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<List<CustomerDTO>> listFilter(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
