package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderStockItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderStockItemRepository extends JpaRepository<OrderStockItem, UUID> {
    List<OrderStockItem> findByOrderStockIdAndOrderItemId(UUID orderId, UUID itemId);
    List<OrderStockItem> findAllByClientIdAndStatusTrue(UUID clientId);
    List<OrderStockItem> findAllByClientIdAndStatusFalse(UUID clientId);
    List<OrderStockItem> findAllByClientIdAndOrderIdAndStatusTrue(UUID clientId,UUID orderId);
    List<OrderStockItem> findAllByClientIdAndOrderIdAndStatusFalse(UUID clientId,UUID orderId);
    OrderStockItem findByOrderStockIdAndSupplierProductIdAndStatusTrue(UUID orderStockId,UUID supplierProductId);
}
