package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestOnboarding;
import com.proyect.masterdata.dto.response.ResponseLogin;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.concurrent.CompletableFuture;

public interface IAuthentication {
    CompletableFuture<ResponseLogin> loginUser(String username, String password);

    CompletableFuture<ResponseSuccess> registerNewClient(RequestOnboarding requestOnboarding)
            throws InternalErrorExceptions, BadRequestExceptions;
}
