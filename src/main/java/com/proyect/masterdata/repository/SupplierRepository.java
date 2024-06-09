package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Supplier;

@Repository
public interface SupplierRepository extends JpaRepository<Supplier, Long> {
    Supplier findByRucAndStatusTrue(String ruc);
    Supplier findByRucAndClientId(String ruc,Long clientId);
    Supplier findByBusinessNameAndClientId(String businessName,Long clientId);
    Supplier findByRucAndClientIdAndStatusTrue(String ruc,Long clientId);
    Supplier findByRucAndClientIdAndStatusFalse(String ruc,Long clientId);
    Supplier findByRuc(String ruc);
    List<Supplier> findAllByClientIdAndStatusTrue(Long clientId);
    List<Supplier> findAllByClientIdAndStatusFalse(Long clientId);
    Supplier findByClientIdAndRucAndStatusTrue(Long clientId,String ruc);
    Supplier findByClientIdAndRucAndStatusFalse(Long clientId,String ruc);
}
