package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Category;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface CategoryRepositoryCustom {
    Page<Category> searchForCategory(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
