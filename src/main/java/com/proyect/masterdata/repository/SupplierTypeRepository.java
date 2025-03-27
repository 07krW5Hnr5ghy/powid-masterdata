package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SupplierType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface SupplierTypeRepository extends JpaRepository<SupplierType, UUID> {
    SupplierType findByNameAndClientIdAndStatusTrue(String name,UUID clientId);
    SupplierType findByNameAndClientIdAndStatusFalse(String name,UUID clientId);
    List<SupplierType> findByNameIn(List<String> names);
    List<SupplierType> findAllByStatusTrue();
}