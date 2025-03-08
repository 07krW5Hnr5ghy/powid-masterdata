package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Brand;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface BrandRepository extends JpaRepository<Brand, UUID> {
    Brand findByNameOrSkuAndClientId(String name,String sku,UUID clientId);
    Brand findByNameOrSkuAndClientIdAndStatusTrue(String name,String sku,UUID clientId);
    Brand findByNameOrSkuAndClientIdAndStatusFalse(String name,String sku,UUID clientId);
    Brand findByNameAndClientIdAndStatusTrue(String name,UUID clientId);
    Brand findByNameAndClientIdAndStatusFalse(String name,UUID clientId);
    List<Brand> findByClientIdAndNameIn(UUID clientId,List<String> namesList);
    List<Brand> findAllByClientIdAndStatusTrue(UUID clientId);
    List<Brand> findAllByClientIdAndStatusFalse(UUID clientId);
    List<Brand> findAllByClientId(UUID clientId);
    Brand findByNameAndClientId(String name,UUID clientId);
}
