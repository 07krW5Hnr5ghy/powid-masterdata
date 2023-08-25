package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.request.RequestColor;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IColor {
    ResponseSuccess save(String name,String user) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions;
    ColorDTO update(RequestColor requestColor) throws BadRequestExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<ColorDTO> list() throws BadRequestExceptions;
    List<ColorDTO> listStatusFalse() throws BadRequestExceptions;
    ColorDTO findByCode(Long code) throws BadRequestExceptions;
    ColorDTO findByName(String name) throws BadRequestExceptions;
    List<ColorDTO> findByUser(String user) throws BadRequestExceptions;
}
