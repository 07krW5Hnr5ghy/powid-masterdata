package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.StoreDTO;
import com.proyect.masterdata.dto.request.RequestStore;
import com.proyect.masterdata.dto.request.RequestStoreSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IStore {
    ResponseSuccess save(String rucClient, RequestStoreSave requestStoreSave, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions;

    ResponseSuccess saveAll(String rucClient, List<RequestStoreSave> storeList, String user)
            throws BadRequestExceptions, InternalErrorExceptions;

    StoreDTO update(RequestStore requestClientChannel) throws BadRequestExceptions, InternalErrorExceptions;

    ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions;

    List<StoreDTO> listClientChannel();

    Page<StoreDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize)
            throws BadRequestExceptions;

    Page<StoreDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;

    StoreDTO findByCode(Long code) throws BadRequestExceptions;
}
