package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.ClosingChannel;
import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.domain.OnboardChannel;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.concurrent.CompletableFuture;

public interface IOnboardChannel {
    CompletableFuture<OnboardChannel> save(Onboard onboard, ClosingChannel closingChannel) throws InternalErrorExceptions;
}
