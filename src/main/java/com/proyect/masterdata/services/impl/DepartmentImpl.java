package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.services.IDepartment;

import java.util.List;

public class DepartmentImpl implements IDepartment {
    @Override
    public ResponseSuccess save(String name) {
        return null;
    }

    @Override
    public ResponseSuccess saveAll(List<String> names) {
        return null;
    }

    @Override
    public DepartmentDTO update(Long code, String name) {
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
    public List<DepartmentDTO> list() {
        return null;
    }

    @Override
    public DepartmentDTO findByCode(Long code) {
        return null;
    }

    @Override
    public DepartmentDTO findByName(String name) {
        return null;
    }
}
