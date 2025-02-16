package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderStock;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderStockRepository extends JpaRepository<OrderStock, UUID> {
    OrderStock findByOrderIdAndClientId(UUID orderId,UUID clientId);
    List<OrderStock> findAllByClientId(UUID clientId);
}
