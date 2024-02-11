package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseItem;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PurchaseItemRepository extends JpaRepository<PurchaseItem, Long> {
    PurchaseItem findByPurchaseIdAndSupplierProductId(Long purchaseId, Long supplierProductId);
    PurchaseItem findBySerial(String serial);
}
