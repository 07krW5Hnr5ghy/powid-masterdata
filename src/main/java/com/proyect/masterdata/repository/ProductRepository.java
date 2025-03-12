package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Product;

@Repository
public interface ProductRepository extends JpaRepository<Product, UUID> {
    List<Product> findAllByClientIdAndStatusTrue(UUID clientId);
    List<Product> findAllByClientId(UUID clientId);
    Product findByIdAndStatusTrue(UUID productId);
    Product findByIdAndStatusFalse(UUID productId);
    Product findByModelIdAndSizeIdAndColorIdAndSubCategoryProductIdAndUnitIdAndClientIdAndStatusTrue(UUID modelId,UUID sizeId,UUID colorId,UUID subCategoryProductId,UUID UnitId,UUID clientId);
    Product findByModelIdAndSizeIdAndColorIdAndSubCategoryProductIdAndUnitIdAndClientId(UUID modelId,UUID sizeId,UUID colorId,UUID subCategoryProductId,UUID UnitId,UUID clientId);
    List<Product> findAllByClientIdAndStatusFalse(UUID clientId);
    List<Product> findByColorNameAndSizeNameAndClientIdAndStatusTrue(String color,String size,UUID clientId);
    List<Product> findByModelNameAndColorNameAndClientIdAndStatusTrue(String model,String color,UUID clientId);
    List<Product> findByModelNameAndSizeNameAndColorNameAndClientIdAndStatusTrue(String model,String size,String color,UUID clientId);
    List<Product> findByModelSkuAndClientIdAndStatusTrue(String modelSku,UUID clientId);
    List<Product> findByModelNameAndClientIdAndStatusTrue(String modelName,UUID clientId);
    Product findByNameAndClientId(String name,UUID clientId);
}
