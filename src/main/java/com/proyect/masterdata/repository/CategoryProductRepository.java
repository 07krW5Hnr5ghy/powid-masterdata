package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.CategoryProduct;

@Repository
public interface CategoryProductRepository extends JpaRepository<CategoryProduct, UUID> {
    CategoryProduct findByNameOrSkuAndClientId(String name,String sku,UUID clientId);
    List<CategoryProduct> findByClientIdAndNameIn(UUID clientId,List<String> names);
    CategoryProduct findByNameAndStatusTrueAndClientId(String name,UUID clientId);
    List<CategoryProduct> findAllByStatusTrueAndClientId(UUID clientId);
    CategoryProduct findByNameAndStatusFalseAndClientId(String name,UUID clientId);
    List<CategoryProduct> findAllByClientId(UUID clientId);
}
