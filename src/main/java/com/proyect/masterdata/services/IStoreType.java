package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.StoreTypeDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IStoreType {
    ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;

    List<StoreTypeDTO> listStoreType() throws BadRequestExceptions;
}
