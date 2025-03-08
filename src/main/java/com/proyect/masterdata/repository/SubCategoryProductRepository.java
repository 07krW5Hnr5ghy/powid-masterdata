package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SubCategoryProduct;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;
@Repository
public interface SubCategoryProductRepository extends JpaRepository<SubCategoryProduct, UUID> {
    SubCategoryProduct findByNameOrSkuAndClientId(String name,String sku,UUID clientId);
    SubCategoryProduct findByNameAndSkuAndClientIdAndStatusTrue(String name,String sku,UUID clientId);
    SubCategoryProduct findByNameAndClientIdAndStatusTrue(String name,UUID clientId);
    List<SubCategoryProduct> findAllByCategoryProductIdAndClientIdAndStatusTrue(UUID categoryProductId,UUID clientId);
}
