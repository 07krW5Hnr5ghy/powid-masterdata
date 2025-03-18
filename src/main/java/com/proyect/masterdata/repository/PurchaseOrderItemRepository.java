package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseOrderItem;
import com.proyect.masterdata.domain.PurchaseOrderItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchaseOrderItemRepository extends JpaRepository<PurchaseOrderItem, UUID> {
    PurchaseOrderItem findByPurchaseOrderIdAndProductId(UUID purchaseOrderId, UUID productId);
    List<PurchaseOrderItem> findAllByClientId(UUID clientId);
    List<PurchaseOrderItem> findAllByClientIdAndPurchaseOrderId(UUID clientId, UUID purchaseOrderId);
    List<PurchaseOrderItem> findAllByPurchaseOrderId(UUID purchaseOrderId);
}
