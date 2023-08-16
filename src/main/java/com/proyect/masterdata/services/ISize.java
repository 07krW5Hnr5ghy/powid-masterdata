package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.request.RequestSize;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface ISize {
    ResponseSuccess save(String name,String user) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names, String user) throws BadRequestExceptions;
    SizeDTO update(RequestSize requestSize) throws BadRequestExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes,String user) throws BadRequestExceptions;
    List<SizeDTO> list() throws BadRequestExceptions;
    List<SizeDTO> listStatusFalse() throws BadRequestExceptions;
    SizeDTO findByCode(Long code) throws BadRequestExceptions;
    SizeDTO findByName(String name) throws BadRequestExceptions;
    List<SizeDTO> findByUser(String user) throws BadRequestExceptions;
}
