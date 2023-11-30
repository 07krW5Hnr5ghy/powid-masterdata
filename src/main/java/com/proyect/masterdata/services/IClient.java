package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ClientDTO;
import com.proyect.masterdata.dto.request.RequestClient;
import com.proyect.masterdata.dto.request.RequestClientSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IClient {
        ResponseSuccess save(RequestClientSave requestClientSave)
                        throws InternalErrorExceptions, BadRequestExceptions;

        ResponseSuccess saveAll(List<RequestClientSave> requestClientSaveList, String user)
                        throws InternalErrorExceptions, BadRequestExceptions;

        ClientDTO update(RequestClient requestClient, String user) throws InternalErrorExceptions, BadRequestExceptions;

        ResponseDelete delete(String ruc, String user) throws InternalErrorExceptions, BadRequestExceptions;

        Page<ClientDTO> list(String ruc, String business, String user, String sort, String sortColumn,
                        Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
}
