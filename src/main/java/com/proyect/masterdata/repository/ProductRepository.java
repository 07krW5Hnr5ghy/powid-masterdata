package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Product;

@Repository
public interface ProductRepository extends JpaRepository<Product, Long> {
    boolean existsBySkuAndStatusTrue(String sku);
    List<Product> findBySkuIn(List<String> skuList);
    Product findBySkuAndStatusTrue(String sku);
    Product findBySkuAndStatusFalse(String sku);
    Product findBySku(String sku);
    List<Product> findAllByClientIdAndStatusTrue(Long clientId);
    List<Product> findAllByClientId(Long clientId);
    List<Product> findAllByClientIdAndStatusFalse(Long clientId);
    List<Product> findAllByClientIdAndModelIdAndStatusFalse(Long clientId,Long modelId);
    List<Product> findByColorNameAndSizeNameAndClientIdAndStatusTrue(String color,String size,Long clientId);
    List<Product> findByModelNameAndColorNameAndClientIdAndStatusTrue(String model,String color,Long clientId);
    List<Product> findByModelNameAndSizeNameAndColorNameAndClientIdAndStatusTrue(String model,String size,String color,Long clientId);
}
