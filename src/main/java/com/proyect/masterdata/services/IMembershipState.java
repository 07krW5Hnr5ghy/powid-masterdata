package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IMembershipState {
    public ResponseSuccess save(String name,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
}
