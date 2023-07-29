package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IMasterList {
    List<MasterListDTO> listRecords() throws BadRequestExceptions;
    ResponseMasterList addRecord(String name) throws BadRequestExceptions;
    ResponseMasterList deleteRecord(Long Id) throws BadRequestExceptions;
    MasterListDTO updateRecord(String name,Long id) throws BadRequestExceptions;
}
