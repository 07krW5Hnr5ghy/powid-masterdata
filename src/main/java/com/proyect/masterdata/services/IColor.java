package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.request.RequestColor;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IColor {
    ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;

    ResponseSuccess saveAll(List<String> names, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;

    ColorDTO update(RequestColor requestColor) throws BadRequestExceptions, InternalErrorExceptions;

    ResponseDelete delete(Long code, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;

    List<ColorDTO> listColor() throws BadRequestExceptions;

    Page<ColorDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize)
            throws BadRequestExceptions;

    Page<ColorDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
}
