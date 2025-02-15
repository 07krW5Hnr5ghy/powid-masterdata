package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Model;

@Repository
public interface ModelRepository extends JpaRepository<Model, UUID> {
    boolean existsByName(String name);
    Model findBySkuAndClientIdAndStatusTrue(String sku,UUID clientId);
    Model findByNameAndClientIdAndStatusTrue(String name,UUID clientId);
    Model findByNameAndClientId(String name,UUID clientId);
    Model findBySkuAndClientId(String sku,UUID clientId);
    Model findBySkuAndClientIdAndStatusFalse(String sku,UUID clientId);
    List<Model> findAllByClientIdAndStatusTrue(UUID clientId);
    List<Model> findAllByClientIdAndStatusFalse(UUID clientId);
    List<Model> findAllByClientIdAndBrandIdAndStatusTrue(UUID clientId,UUID brandId);
    List<Model> findAllByClientId(UUID clientId);
}
