package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturnItem;
import org.springframework.data.jpa.repository.JpaRepository;

public interface OrderReturnItemRepository extends JpaRepository<OrderReturnItem,Long> {
    OrderReturnItem findBySupplierProductIdAndProductIdAndOrderId(Long supplierProductId,Long productId,Long orderId);
}
