package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderStock;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderStockRepository extends JpaRepository<OrderStock,Long> {
    List<OrderStock> findByOrderIdAndItemId(Long orderId, Long itemId);
}
