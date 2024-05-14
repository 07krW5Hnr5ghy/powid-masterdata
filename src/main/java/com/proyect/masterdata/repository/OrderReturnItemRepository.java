package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturnItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface OrderReturnItemRepository extends JpaRepository<OrderReturnItem,Long> {
    OrderReturnItem findByClientIdAndOrderIdAndSupplierProductIdAndStatusTrue(Long clientId,Long orderId,Long supplierProductId);
    List<OrderReturnItem> findAllByClientIdAndOrderIdAndStatusTrue(Long clientId,Long orderId);
    List<OrderReturnItem> findAllByClientIdAndStatusTrue(Long clientId);
    OrderReturnItem findBySupplierProductIdAndOrderId(Long supplierProductId,Long orderId);
}
