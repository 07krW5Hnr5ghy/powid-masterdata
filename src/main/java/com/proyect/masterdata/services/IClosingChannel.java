package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.ClosingChannelDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IClosingChannel {
    ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;

    List<ClosingChannelDTO> listClosingChannel() throws BadRequestExceptions;
}
