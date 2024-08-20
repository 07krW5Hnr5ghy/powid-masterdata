package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.CategoryProduct;

import java.util.Date;

@Repository
public interface CategoryProductRepositoryCustom {
    public Page<CategoryProduct> searchForCategoryProduct(
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
            Boolean status);
}
