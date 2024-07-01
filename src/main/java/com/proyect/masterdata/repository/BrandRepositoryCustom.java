package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.User;
import org.springframework.stereotype.Repository;
import org.springframework.data.domain.Page;

import java.util.Date;
import java.util.List;

@Repository
public interface BrandRepositoryCustom {
    Page<Brand> searchForBrand(
            Long clientId,
            List<String> names,
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
