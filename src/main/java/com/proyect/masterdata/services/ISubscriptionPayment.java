package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestSubscriptionPayment;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.concurrent.CompletableFuture;

public interface ISubscriptionPayment {
    CompletableFuture<String> send(RequestSubscriptionPayment requestSubscriptionPayment, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> activateDemo(String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
