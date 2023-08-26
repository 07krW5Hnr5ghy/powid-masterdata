package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.request.RequestSizeType;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface ISizeType {
    ResponseSuccess save(String name, String user) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> name,String user) throws BadRequestExceptions,InternalErrorExceptions;
    SizeTypeDTO update(RequestSizeType requestSizeType) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<SizeTypeDTO> listSizeType() throws BadRequestExceptions;
    List<SizeTypeDTO> listStatusFalse() throws BadRequestExceptions;
    SizeTypeDTO findByCode(Long code) throws BadRequestExceptions;
}
