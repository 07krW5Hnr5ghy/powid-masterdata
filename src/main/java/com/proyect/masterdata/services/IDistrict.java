package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.request.RequestDistrict;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IDistrict {
    ResponseSuccess save(String name, String user, Long codeProvince) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names,String user, Long codeProvince) throws BadRequestExceptions;
    DistrictDTO update(RequestDistrict requestDistrict) throws BadRequestExceptions;
    ResponseDelete delete(Long code, String user) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes, String user) throws BadRequestExceptions;
    List<DistrictDTO> list() throws BadRequestExceptions;
    List<DistrictDTO> listStatusFalse() throws BadRequestExceptions;
    DistrictDTO findByCode(Long code) throws BadRequestExceptions;
    DistrictDTO findByName(String name) throws BadRequestExceptions;
    List<DistrictDTO> findByUser(String user) throws BadRequestExceptions;
    List<DistrictDTO> findAllProvinceId(Long codeProvince) throws BadRequestExceptions;
    List<DistrictDTO> findAllProvinceName(String nameProvincet) throws BadRequestExceptions;
}
