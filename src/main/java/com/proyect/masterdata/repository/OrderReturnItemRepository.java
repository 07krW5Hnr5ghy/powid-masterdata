package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturnItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface OrderReturnItemRepository extends JpaRepository<OrderReturnItem, UUID> {
    OrderReturnItem findByClientIdAndOrderReturnIdAndSupplierProductIdAndStatusTrue(UUID clientId,UUID orderId,UUID supplierProductId);
    List<OrderReturnItem> findAllByClientIdAndOrderReturnIdAndStatusTrue(UUID clientId,UUID orderId);
    List<OrderReturnItem> findAllByClientIdAndStatusTrue(UUID clientId);
    OrderReturnItem findBySupplierProductIdAndOrderReturnIdAndStatusTrue(UUID supplierProductId,UUID orderId);
    OrderReturnItem findBySupplierProductIdAndOrderReturnIdAndStatusFalse(UUID supplierProductId,UUID orderId);
}
