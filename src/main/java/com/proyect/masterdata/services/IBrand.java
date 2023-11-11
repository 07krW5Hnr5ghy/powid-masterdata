package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.dto.BrandDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

import org.springframework.data.domain.Page;

public interface IBrand {

    ResponseSuccess save(String name, String user) throws InternalErrorExceptions, BadRequestExceptions;

    ResponseSuccess saveAll(List<String> namesList, String user) throws InternalErrorExceptions, BadRequestExceptions;

    ResponseDelete delete(String name, String user) throws InternalErrorExceptions, BadRequestExceptions;

    Page<BrandDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize)
            throws InternalErrorExceptions, BadRequestExceptions;

    Page<BrandDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize)
            throws InternalErrorExceptions, BadRequestExceptions;
}
