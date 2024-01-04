package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ModuleDTO;
import com.proyect.masterdata.dto.request.RequestModule;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IModule {
        ResponseSuccess save(String name, double price, String tokenUser)
                        throws BadRequestExceptions, InternalErrorExceptions;

        ResponseSuccess saveAll(List<RequestModule> moduleList, String tokenUser)
                        throws BadRequestExceptions, InternalErrorExceptions;

        ModuleDTO update(RequestModule requestModule, String tokenUser)
                        throws BadRequestExceptions, InternalErrorExceptions;

        ResponseDelete delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;

        List<ModuleDTO> listModule() throws BadRequestExceptions;

        Page<ModuleDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize)
                        throws BadRequestExceptions;

        Page<ModuleDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize) throws BadRequestExceptions;

        ModuleDTO findByCode(Long code) throws BadRequestExceptions;
}
