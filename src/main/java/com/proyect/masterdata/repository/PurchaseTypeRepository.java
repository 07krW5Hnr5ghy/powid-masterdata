package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchaseTypeRepository extends JpaRepository<PurchaseType, UUID> {
    PurchaseType findByName(String name);
    PurchaseType findByNameAndStatusTrue(String name);
    PurchaseType findByNameAndStatusFalse(String name);
    List<PurchaseType> findByNameIn(List<String> names);
    List<PurchaseType> findAllByStatusTrue();
}
