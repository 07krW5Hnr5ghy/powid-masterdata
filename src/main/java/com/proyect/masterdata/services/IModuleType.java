package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestModuleTypeSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IModuleType {
    ResponseSuccess save(String userType,String module,String user) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseSuccess saveAll(List<RequestModuleTypeSave> requestModuleTypeSaveList,String user) throws InternalErrorExceptions,BadRequestExceptions;
}
