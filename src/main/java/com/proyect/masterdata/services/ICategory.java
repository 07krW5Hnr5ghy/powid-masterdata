package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.request.RequestCategory;
import com.proyect.masterdata.dto.request.RequestCreateCategory;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface ICategory {
    ResponseSuccess save(String name, String description,String user) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseSuccess saveAll(List<RequestCreateCategory> categories,String user) throws BadRequestExceptions,InternalErrorExceptions;
    CategoryDTO update(RequestCategory requestCategory) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<CategoryDTO> listCategory() throws BadRequestExceptions;
    Page<CategoryDTO> list(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    Page<CategoryDTO> listStatusFalse(String name,String user,String sort,String sortColumn,Integer pageNumber,Integer pageSize) throws BadRequestExceptions;
    CategoryDTO findByCode(Long code) throws BadRequestExceptions;
}
