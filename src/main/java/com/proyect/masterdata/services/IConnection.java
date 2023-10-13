package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ConnectionDTO;
import com.proyect.masterdata.dto.PaymentTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IConnection {
    ResponseSuccess save(String url,String user) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseSuccess saveAll(List<String> urls,String user) throws InternalErrorExceptions,BadRequestExceptions;
    ResponseDelete delete(String url,String user) throws InternalErrorExceptions,BadRequestExceptions;
    Page<ConnectionDTO> list(String url, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<ConnectionDTO> listStatusFalse(String url,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
}
