package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.dto.BrandDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

import org.springframework.data.domain.Page;

public interface IBrand {
        ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        ResponseSuccess saveAll(List<String> namesList, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        Page<BrandDTO> list(String name, String tokenUser, String sort, String sortColumn, Integer pageNumber,
                        Integer pageSize)
                        throws InternalErrorExceptions, BadRequestExceptions;
        Page<BrandDTO> listStatusFalse(String name, String tokenUser, String sort, String sortColumn,
                        Integer pageNumber,
                        Integer pageSize)
                        throws InternalErrorExceptions, BadRequestExceptions;
        ResponseSuccess activate(String name,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
