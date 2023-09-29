package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.dto.request.RequestModule;
import com.proyect.masterdata.dto.request.RequestModuleSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IModule {
    ResponseSuccess save(String name,double price,int moduleStatus, String user) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseSuccess saveAll(List<RequestModuleSave> moduleList, String user) throws BadRequestExceptions, InternalErrorExceptions;
    ModuleDTO update(RequestModule requestModule) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<ModuleDTO> listModule();
    Page<ModuleDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<ModuleDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    ModuleDTO findByCode(Long code) throws BadRequestExceptions;
}
