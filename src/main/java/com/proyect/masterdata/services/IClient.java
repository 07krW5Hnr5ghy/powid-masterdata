package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ClientDTO;
import com.proyect.masterdata.dto.request.RequestClient;
import com.proyect.masterdata.dto.request.RequestClientSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IClient {
    ResponseSuccess save(RequestClientSave requestClientSave,String user) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseSuccess saveAll(List<RequestClientSave> requestClientSaveList,String user) throws InternalErrorExceptions,BadRequestExceptions;
    ClientDTO update(RequestClient requestClient) throws InternalErrorExceptions,BadRequestExceptions;
}
