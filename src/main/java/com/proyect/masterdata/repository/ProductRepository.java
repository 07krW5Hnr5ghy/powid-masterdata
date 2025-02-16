package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Product;

@Repository
public interface ProductRepository extends JpaRepository<Product, UUID> {
    boolean existsBySkuAndStatusTrue(String sku);
    List<Product> findBySkuIn(List<String> skuList);
    Product findBySkuAndStatusTrue(String sku);
    Product findBySkuAndStatusFalse(String sku);
    Product findBySku(String sku);
    List<Product> findAllByClientIdAndStatusTrue(UUID clientId);
    List<Product> findAllByClientId(UUID clientId);
    List<Product> findAllByClientIdAndStatusFalse(UUID clientId);
    List<Product> findByColorNameAndSizeNameAndClientIdAndStatusTrue(String color,String size,UUID clientId);
    List<Product> findByModelNameAndColorNameAndClientIdAndStatusTrue(String model,String color,UUID clientId);
    List<Product> findByModelNameAndSizeNameAndColorNameAndClientIdAndStatusTrue(String model,String size,String color,UUID clientId);
    List<Product> findByModelSkuAndClientIdAndStatusTrue(String modelSku,UUID clientId);
}
