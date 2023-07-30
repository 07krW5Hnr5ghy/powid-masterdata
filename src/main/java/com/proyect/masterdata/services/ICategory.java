package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

public interface ICategory {
    List<CategoryDTO> listRecords() throws BadRequestExceptions;
    ResponseMasterList addRecord(String name,String description) throws BadRequestExceptions;
    ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions;
    CategoryDTO updateRecord(String name,Long id,String description) throws BadRequestExceptions;
}
