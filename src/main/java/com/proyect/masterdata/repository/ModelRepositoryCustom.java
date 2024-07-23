package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.User;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.Model;

import java.util.Date;
import java.util.List;

@Repository
public interface ModelRepositoryCustom {
    Page<Model> searchForModel(
            Long clientId,
            List<String> names,
            List<Long> brandIds,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
