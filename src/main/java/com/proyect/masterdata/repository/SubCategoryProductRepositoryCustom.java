package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CategoryProduct;
import com.proyect.masterdata.domain.SubCategoryProduct;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.List;

@Repository
public interface SubCategoryProductRepositoryCustom {
    Page<SubCategoryProduct> searchForSubCategoryProduct(
            String name,
            String sku,
            String user,
            String categoryProduct,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
