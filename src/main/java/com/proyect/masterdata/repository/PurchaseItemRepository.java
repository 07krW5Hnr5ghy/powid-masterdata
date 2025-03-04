package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.PurchaseItem;

import java.util.List;
import java.util.UUID;

public interface PurchaseItemRepository extends JpaRepository<PurchaseItem, UUID> {
    PurchaseItem findByPurchaseIdAndProductId(UUID purchaseId, UUID productId);
    List<PurchaseItem> findAllByClientId(UUID clientId);
    List<PurchaseItem> findAllByClientIdAndPurchaseId(UUID clientId, UUID purchaseId);
}
