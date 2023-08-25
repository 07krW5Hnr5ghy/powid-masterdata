package com.proyect.masterdata.services;
import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestProvince;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IProvince {
    ResponseSuccess save(String name, String user, Long codeDepartment) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> names,String user, Long codeDepartment) throws BadRequestExceptions, InternalErrorExceptions;
    ProvinceDTO update(RequestProvince requestProvince) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions;
    ResponseDelete deleteAll(List<Long> codes, String user) throws BadRequestExceptions;
    List<ProvinceDTO> list() throws BadRequestExceptions;
    List<ProvinceDTO> listStatusFalse() throws BadRequestExceptions;
    ProvinceDTO findByCode(Long code) throws BadRequestExceptions;
    ProvinceDTO findByName(String name) throws BadRequestExceptions;
    List<ProvinceDTO> findByUser(String user) throws BadRequestExceptions;
    List<ProvinceDTO> findAllDepartmentId(Long codeDepartment) throws BadRequestExceptions;
    List<ProvinceDTO> findAllDepartmentName(String nameDepartment) throws BadRequestExceptions;
}
