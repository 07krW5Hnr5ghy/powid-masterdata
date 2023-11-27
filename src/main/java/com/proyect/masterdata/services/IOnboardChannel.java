package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.ClosingChannel;
import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.domain.OnboardChannel;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IOnboardChannel {
    OnboardChannel save(Onboard onboard, ClosingChannel closingChannel) throws InternalErrorExceptions;
}
