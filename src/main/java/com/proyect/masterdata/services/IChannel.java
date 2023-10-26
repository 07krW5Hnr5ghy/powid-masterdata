package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ChannelDTO;
import com.proyect.masterdata.dto.ChannelListDTO;
import com.proyect.masterdata.dto.request.RequestChannelSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IChannel {
    ResponseSuccess save(RequestChannelSave requestChannelSave, String user) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseSuccess saveAll(List<RequestChannelSave> requestChannelSaveList,String user) throws InternalErrorExceptions,BadRequestExceptions;
    ChannelDTO update(String name,Integer months,String user) throws InternalErrorExceptions,BadRequestExceptions;
    ResponseDelete delete(String name,String user) throws InternalErrorExceptions,BadRequestExceptions;
    Page<ChannelListDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<ChannelListDTO> listStatusFalse(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
}
