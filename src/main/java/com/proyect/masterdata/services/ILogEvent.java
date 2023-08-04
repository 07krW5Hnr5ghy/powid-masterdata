package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.LogEventDTO;
import com.proyect.masterdata.dto.request.RequestLogEvent;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface ILogEvent {
    ResponseSuccess save(String name) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions;
    LogEventDTO update(RequestLogEvent requestLogEvent) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<LogEventDTO> list() throws BadRequestExceptions;
    LogEventDTO findByCode(Long code) throws BadRequestExceptions;
    LogEventDTO findByName(String name) throws BadRequestExceptions;
}
