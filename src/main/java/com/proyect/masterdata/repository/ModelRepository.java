package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Model;

@Repository
public interface ModelRepository extends JpaRepository<Model, Long> {
    boolean existsByName(String name);
    Model findBySkuAndClientIdAndStatusTrue(String sku,Long clientId);
    Model findByNameAndClientIdAndStatusTrue(String name,Long clientId);
    Model findBySkuAndClientId(String sku,Long clientId);
    Model findBySkuAndClientIdAndStatusFalse(String sku,Long clientId);
    List<Model> findByClientIdAndSkuIn(Long clientId,List<String> skus);
    List<Model> findAllByClientIdAndStatusTrue(Long clientId);
    List<Model> findAllByClientIdAndStatusFalse(Long clientId);
    List<Model> findAllByClientIdAndBrandIdAndStatusTrue(Long clientId,Long brandId);
    List<Model> findAllByClientId(Long clientId);
}
