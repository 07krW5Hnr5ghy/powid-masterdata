package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface PurchaseItemRepository extends JpaRepository<PurchaseItem, Long> {
    PurchaseItem findByPurchaseIdAndSupplierProductId(Long purchaseId, Long supplierProductId);
    List<PurchaseItem> findAllByClientIdAndStatusTrue(Long clientId);
    List<PurchaseItem> findAllByClientIdAndStatusFalse(Long clientId);
    List<PurchaseItem> findAllByClientIdAndPurchaseIdAndStatusTrue(Long clientId,Long purchaseId);
    List<PurchaseItem> findAllByClientIdAndPurchaseIdAndStatusFalse(Long clientId,Long purchaseId);
}
