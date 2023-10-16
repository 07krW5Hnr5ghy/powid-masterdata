package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ClientChannelDTO;
import com.proyect.masterdata.dto.request.RequestClientChannel;
import com.proyect.masterdata.dto.request.RequestClientChannelSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IClientChannel {
    ResponseSuccess save(RequestClientChannelSave requestClientChannelSave,String user) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseSuccess saveAll(List<RequestClientChannelSave> ClientChannelList, String user) throws BadRequestExceptions, InternalErrorExceptions;
    ClientChannelDTO update(RequestClientChannel requestClientChannel) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<ClientChannelDTO> listClientChannel();
    Page<ClientChannelDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<ClientChannelDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    ClientChannelDTO findByCode(Long code) throws BadRequestExceptions;
}
