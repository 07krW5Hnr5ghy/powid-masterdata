package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.dto.OrderDTO;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderItemRepository extends JpaRepository<OrderItem,Long> {
    List<OrderItem> findAllByOrderId(Long orderId);
    List<OrderItem> findAllByOrderIdAndStatusTrue(Long orderId);
    OrderItem findByIdAndOrderId(Long itemId, Long orderId);
    OrderItem findByOrderIdAndProductId(Long orderId,Long productId);
    List<OrderItem> findAllByClientIdAndStatusTrue(Long clientId);
    List<OrderItem> findAllByClientIdAndOrderIdAndStatusTrue(Long clientId,Long orderId);
}
