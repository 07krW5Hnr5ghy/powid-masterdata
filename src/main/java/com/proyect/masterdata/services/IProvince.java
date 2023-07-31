package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;

import java.util.List;

public interface IProvince {
    ResponseSuccess save(String name);
    ResponseSuccess saveAll(List<String> names);
    ProvinceDTO update(Long code, String name);
    ResponseDelete delete(Long code);
    ResponseDelete deleteAll(List<Long> codes);
    List<ProvinceDTO> list();
    ProvinceDTO findByCode(Long code);
    ProvinceDTO findByName(String name);
}
