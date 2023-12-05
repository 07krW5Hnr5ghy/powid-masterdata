package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.dto.request.RequestOnboard;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IOnboard {
    Onboard save(RequestOnboard requestOnboard) throws InternalErrorExceptions, BadRequestExceptions;

    List<Onboard> listOnboard() throws BadRequestExceptions;
}
