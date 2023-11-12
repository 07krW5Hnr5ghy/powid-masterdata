package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.Model;

@Repository
public interface ModelRepositoryCustom {
    Page<Model> searchForModel(
            String name,
            Brand Brand,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
