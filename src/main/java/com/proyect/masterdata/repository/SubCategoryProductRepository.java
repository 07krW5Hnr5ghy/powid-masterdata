package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SubCategoryProduct;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface SubCategoryProductRepository extends JpaRepository<SubCategoryProduct, UUID> {
    SubCategoryProduct findByNameOrSku(String name,String sku);
    SubCategoryProduct findByNameAndSkuAndStatusTrue(String name,String sku);
    SubCategoryProduct findByNameAndStatusTrue(String name);
}
