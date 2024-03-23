package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockReplenishmentRepository extends JpaRepository<StockReplenishment,Long> {
    StockReplenishment findByOrderId(Long orderId);
    List<StockReplenishment> findAllByClientIdAndStatusTrue(Long clientId);
    List<StockReplenishment> findAllByClientIdAndStatusFalse(Long clientId);
}
