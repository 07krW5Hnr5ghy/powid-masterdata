package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Category;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.Date;

@Repository
public interface CategoryRepositoryCustom {
    Page<Category> searchForCategory(
            String name,
            String user,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
