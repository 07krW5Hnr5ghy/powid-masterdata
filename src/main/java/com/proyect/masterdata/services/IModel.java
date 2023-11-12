package com.proyect.masterdata.services;

import java.util.List;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ModelDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IModel {

    ResponseSuccess save(String name, String brand, String user) throws InternalErrorExceptions, BadRequestExceptions;

    ResponseSuccess saveAll(List<String> names, String brand, String user)
            throws InternalErrorExceptions, BadRequestExceptions;

    ResponseDelete delete(String name, String user) throws InternalErrorExceptions, BadRequestExceptions;

    Page<ModelDTO> list(String name, String user, String sort, String columnSort, Integer pageNumber, Integer pageSize);

    Page<ModelDTO> listStatusFalse(String name, String user, String sort, String columnSort, Integer pageNumber,
            Integer pageSize);

}
