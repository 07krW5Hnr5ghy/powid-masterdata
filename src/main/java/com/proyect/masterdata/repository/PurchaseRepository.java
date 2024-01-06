package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.Purchase;

public interface PurchaseRepository extends JpaRepository<Purchase, Long> {
    Purchase findBySerialAndSupplierProductId(String serial, Long supplierProductId);
}
