package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.dto.request.RequestCustomer;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.concurrent.CompletableFuture;

public interface ICustomer {
    CompletableFuture<ResponseSuccess> save(Ordering ordering, RequestCustomer requestCustomer, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;

}
