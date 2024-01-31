package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.User;
import org.springframework.stereotype.Repository;
import org.springframework.data.domain.Page;

@Repository
public interface BrandRepositoryCustom {
    Page<Brand> searchForBrand(
            String name,
            Long clientId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
