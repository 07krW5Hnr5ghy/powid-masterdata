package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.domain.OnboardStore;
import com.proyect.masterdata.domain.Store;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.concurrent.CompletableFuture;

public interface IOnboardStore {
    CompletableFuture<OnboardStore> save(Store store, Onboard onboard) throws InternalErrorExceptions;
}
