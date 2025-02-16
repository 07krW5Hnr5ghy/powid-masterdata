package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockReplenishmentRepository extends JpaRepository<StockReplenishment, UUID> {
    StockReplenishment findByOrderId(UUID orderId);
    List<StockReplenishment> findAllByClientIdAndStatusTrue(UUID clientId);
    List<StockReplenishment> findAllByClientIdAndStatusFalse(UUID clientId);
}
