package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Product;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface ProductRepositoryCustom {
    Page<Product> searchForProduct(
            UUID clientId,
            String productSku,
            String product,
            String model,
            String brand,
            String size,
            String categoryProduct,
            String subCategoryProduct,
            String color,
            String unit,
            Boolean pictureFlag,
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
