package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IMembership {
    ResponseSuccess save(Long id,String module,String user) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseSuccess saveAll(Long id, List<String> moduleList,String user) throws InternalErrorExceptions,BadRequestExceptions;
}
