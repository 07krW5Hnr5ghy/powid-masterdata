package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Supplier;

@Repository
public interface SupplierRepository extends JpaRepository<Supplier, UUID> {
    Supplier findByRucAndStatusTrue(String ruc);
    Supplier findBySkuAndClientId(String sku,UUID clientId);
    Supplier findBySkuOrRucAndClientId(String sku,String ruc,UUID clientId);
    Supplier findByRucAndClientId(String ruc,UUID clientId);
    Supplier findByBusinessNameAndClientId(String businessName,UUID clientId);
    Supplier findByRucAndClientIdAndStatusTrue(String ruc,UUID clientId);
    Supplier findByRucAndClientIdAndStatusFalse(String ruc,UUID clientId);
    Supplier findByRuc(String ruc);
    List<Supplier> findAllByClientIdAndStatusTrue(UUID clientId);
    List<Supplier> findAllByClientIdAndStatusFalse(UUID clientId);
    Supplier findByClientIdAndRucAndStatusTrue(UUID clientId,String ruc);
    Supplier findByClientIdAndRucAndStatusFalse(UUID clientId,String ruc);
    List<Supplier> findAllByClientId(UUID clientId);
    List<Supplier> findByRucIn(List<String> ruc);
}
