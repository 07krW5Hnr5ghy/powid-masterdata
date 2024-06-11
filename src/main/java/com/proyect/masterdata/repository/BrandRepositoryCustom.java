package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.User;
import org.springframework.stereotype.Repository;
import org.springframework.data.domain.Page;

import java.util.Date;

@Repository
public interface BrandRepositoryCustom {
    Page<Brand> searchForBrand(
            String name,
            Long clientId,
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
