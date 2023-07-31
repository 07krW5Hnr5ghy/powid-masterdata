package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;

import java.util.List;

public interface IDistrict {
    ResponseSuccess save(String name);
    ResponseSuccess saveAll(List<String> names);
    DistrictDTO update(Long code, String name);
    ResponseDelete delete(Long code);
    ResponseDelete deleteAll(List<Long> codes);
    List<DistrictDTO> list();
    DistrictDTO findByCode(Long code);
    DistrictDTO findByName(String name);
}
