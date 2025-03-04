package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SupplyOrderItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface SupplyOrderItemRepository extends JpaRepository<SupplyOrderItem, UUID> {
    SupplyOrderItem findByPurchaseIdAndProductId(UUID purchaseId, UUID productId);
    List<SupplyOrderItem> findAllByClientId(UUID clientId);
    List<SupplyOrderItem> findAllByClientIdAndPurchaseId(UUID clientId, UUID purchaseId);
}
