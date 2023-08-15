package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.request.RequestColor;
import com.proyect.masterdata.dto.request.RequestColorSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IColor {
    ResponseSuccess save(String name,String user) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<RequestColorSave> requestColorSaveList) throws BadRequestExceptions;
    ColorDTO update(RequestColor requestColor) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<ColorDTO> list() throws BadRequestExceptions;
    ColorDTO findByCode(Long code) throws BadRequestExceptions;
    ColorDTO findByName(String name) throws BadRequestExceptions;
}
