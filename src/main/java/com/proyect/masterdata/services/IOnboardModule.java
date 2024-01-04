package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.domain.OnboardModule;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IOnboardModule {
    OnboardModule save(Onboard onboard, Module module) throws InternalErrorExceptions;
}
