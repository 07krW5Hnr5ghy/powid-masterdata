package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishmentItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockReplenishmentItemRepository extends JpaRepository<StockReplenishmentItem, UUID> {
    List<StockReplenishmentItem> findAllByClientIdAndStatusTrue(UUID clientId);
    List<StockReplenishmentItem> findAllByClientIdAndStatusFalse(UUID clientId);
    StockReplenishmentItem findByOrderIdAndProductId(UUID orderId,UUID productId);
    StockReplenishmentItem findByOrderIdAndProductIdAndStatusTrue(UUID orderId,UUID productId);
    StockReplenishmentItem findByOrderIdAndProductIdAndStatusFalse(UUID orderId,UUID productId);
}
