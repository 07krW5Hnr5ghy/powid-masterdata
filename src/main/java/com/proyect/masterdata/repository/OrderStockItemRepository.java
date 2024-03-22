package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderStockItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderStockItemRepository extends JpaRepository<OrderStockItem,Long> {
    List<OrderStockItem> findByOrderStockIdAndItemId(Long orderId, Long itemId);
    List<OrderStockItem> findAllByClientIdAndStatusTrue(Long clientId);
    List<OrderStockItem> findAllByClientIdAndStatusFalse(Long clientId);
}
