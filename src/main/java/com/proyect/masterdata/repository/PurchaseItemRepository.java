package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.PurchaseItem;

import java.util.List;

public interface PurchaseItemRepository extends JpaRepository<PurchaseItem, Long> {
    PurchaseItem findByPurchaseIdAndSupplierProductId(Long shipmentId, Long supplierProductId);
    List<PurchaseItem> findAllByClientId(Long clientId);
    List<PurchaseItem> findAllByClientIdAndPurchaseId(Long clientId, Long shipmentId);
}
