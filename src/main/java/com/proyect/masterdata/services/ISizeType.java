package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.request.RequestSizeType;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface ISizeType {
    ResponseSuccess save(String name, String user) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> name,String user) throws BadRequestExceptions;
    SizeTypeDTO update(RequestSizeType requestSizeType) throws BadRequestExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes,String user) throws BadRequestExceptions;
    List<SizeTypeDTO> list() throws BadRequestExceptions;
    List<SizeTypeDTO> listStatusFalse() throws BadRequestExceptions;
    SizeTypeDTO findByCode(Long code) throws BadRequestExceptions;
    SizeTypeDTO findByName(String name) throws BadRequestExceptions;
    List<SizeTypeDTO> findByUser(String user) throws BadRequestExceptions;
}
