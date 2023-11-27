package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.domain.OnboardStore;
import com.proyect.masterdata.domain.Store;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IOnboardStore {

    OnboardStore save(Store store, Onboard onboard) throws InternalErrorExceptions;
}
