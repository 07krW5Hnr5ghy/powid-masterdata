package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.request.RequestCategory;
import com.proyect.masterdata.dto.request.RequestCategorySave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface ICategory {
    ResponseSuccess save(String name, String description) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<RequestCategorySave> requestCategoryList) throws BadRequestExceptions;
    CategoryDTO update(RequestCategory requestCategory) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<CategoryDTO> list() throws BadRequestExceptions;
    CategoryDTO findByCode(Long code) throws BadRequestExceptions;
    CategoryDTO findByName(String name) throws BadRequestExceptions;
}
