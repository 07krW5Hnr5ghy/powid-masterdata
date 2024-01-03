package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface ISizeType {
    ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;

    ResponseSuccess saveAll(List<String> name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;

    ResponseDelete delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;

    List<SizeTypeDTO> listSizeType() throws BadRequestExceptions;

    Page<SizeTypeDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;

    Page<SizeTypeDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
}
