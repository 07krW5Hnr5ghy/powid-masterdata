package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.services.IDistrict;

import java.util.List;

public class DistrictImpl implements IDistrict {
    @Override
    public ResponseSuccess save(String name) {
        return null;
    }

    @Override
    public ResponseSuccess saveAll(List<String> names) {
        return null;
    }

    @Override
    public DistrictDTO update(Long code, String name) {
        return null;
    }

    @Override
    public ResponseDelete delete(Long code) {
        return null;
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes) {
        return null;
    }

    @Override
    public List<DistrictDTO> list() {
        return null;
    }

    @Override
    public DistrictDTO findByCode(Long code) {
        return null;
    }

    @Override
    public DistrictDTO findByName(String name) {
        return null;
    }
}
