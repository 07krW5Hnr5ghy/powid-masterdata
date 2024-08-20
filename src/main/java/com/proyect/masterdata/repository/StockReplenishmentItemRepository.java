package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishmentItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockReplenishmentItemRepository extends JpaRepository<StockReplenishmentItem,Long> {
    List<StockReplenishmentItem> findAllByClientIdAndStatusTrue(Long clientId);
    List<StockReplenishmentItem> findAllByClientIdAndStatusFalse(Long clientId);
    StockReplenishmentItem findByOrderIdAndProductId(Long orderId,Long productId);
    StockReplenishmentItem findByOrderIdAndProductIdAndStatusTrue(Long orderId,Long productId);
    StockReplenishmentItem findByOrderIdAndProductIdAndStatusFalse(Long orderId,Long productId);
}
