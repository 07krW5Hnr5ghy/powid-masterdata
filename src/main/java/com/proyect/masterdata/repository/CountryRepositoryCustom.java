package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Country;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface CountryRepositoryCustom {
    Page<Country> searchForCountry(
            String name,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
