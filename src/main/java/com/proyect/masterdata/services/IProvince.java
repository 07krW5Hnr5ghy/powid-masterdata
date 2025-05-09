package com.proyect.masterdata.services;
import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestProvince;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IProvince {
    ResponseSuccess save(String name, String user, Long codeDepartment) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> names,String user, Long codeDepartment) throws BadRequestExceptions, InternalErrorExceptions;
    ProvinceDTO update(RequestProvince requestProvince) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<ProvinceDTO> listProvince() throws BadRequestExceptions;
    Page<ProvinceDTO> list(String name, String user,Long codeDepartment, String nameDepartment, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<ProvinceDTO> listStatusFalse(String name, String user, Long codeDepartment, String nameDepartment, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    ProvinceDTO findByCode(Long code) throws BadRequestExceptions ;
}
