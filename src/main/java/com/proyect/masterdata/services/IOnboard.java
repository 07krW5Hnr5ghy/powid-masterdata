package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.dto.OnboardingDTO;
import com.proyect.masterdata.dto.request.RequestOnboard;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IOnboard {
    CompletableFuture<Onboard> save(RequestOnboard requestOnboard) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<OnboardingDTO>> listOnboard() throws BadRequestExceptions;
}
