package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.request.RequestSize;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface ISize {
    ResponseSuccess save(String name,String user,Long codeSizeType) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> names, String user,Long codeSizeType) throws BadRequestExceptions,InternalErrorExceptions;
    SizeDTO update(RequestSize requestSize) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<SizeDTO> listSize() throws BadRequestExceptions;
    Page<SizeDTO> list(String name, String user, Long codeSizeType, String nameSizeType, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<SizeDTO> listStatusFalse(String name, String user, Long codeSizeType, String nameSizeType, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    SizeDTO findByCode(Long code) throws BadRequestExceptions;
    List<SizeDTO> findAllSizeTypeId(Long codeSizeType) throws BadRequestExceptions;
    List<SizeDTO> findAllSizeTypeName(String nameSizeType) throws BadRequestExceptions;
}
