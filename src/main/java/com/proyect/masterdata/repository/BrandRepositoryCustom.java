package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.User;
import org.springframework.stereotype.Repository;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Repository
public interface BrandRepositoryCustom {
    Page<Brand> searchForBrand(
            UUID clientId,
            List<String> names,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
