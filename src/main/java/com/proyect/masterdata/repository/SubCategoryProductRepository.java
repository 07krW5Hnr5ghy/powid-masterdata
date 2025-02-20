package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SubCategoryProduct;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;
@Repository
public interface SubCategoryProductRepository extends JpaRepository<SubCategoryProduct, UUID> {
    SubCategoryProduct findByNameOrSku(String name,String sku);
    SubCategoryProduct findByNameAndSkuAndStatusTrue(String name,String sku);
    SubCategoryProduct findByNameAndStatusTrue(String name);
    List<SubCategoryProduct> findAllByCategoryProductIdAndStatusTrue(UUID categoryProductId);
}
