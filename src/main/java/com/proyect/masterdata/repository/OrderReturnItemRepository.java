package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturnItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface OrderReturnItemRepository extends JpaRepository<OrderReturnItem,Long> {
    OrderReturnItem findByClientIdAndOrderReturnIdAndSupplierProductIdAndStatusTrue(Long clientId,Long orderId,Long supplierProductId);
    List<OrderReturnItem> findAllByClientIdAndOrderReturnIdAndStatusTrue(Long clientId,Long orderId);
    List<OrderReturnItem> findAllByClientIdAndStatusTrue(Long clientId);
    OrderReturnItem findBySupplierProductIdAndOrderReturnIdAndStatusTrue(Long supplierProductId,Long orderId);
    OrderReturnItem findBySupplierProductIdAndOrderReturnIdAndStatusFalse(Long supplierProductId,Long orderId);
}
