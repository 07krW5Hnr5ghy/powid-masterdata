package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface StockReplenishmentRepository extends JpaRepository<StockReplenishment,Long> {
    StockReplenishment findByOrderId(Long orderId);
}
