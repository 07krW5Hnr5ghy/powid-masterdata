package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturn;
import com.proyect.masterdata.domain.StockReturnItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockReturnItemRepository extends JpaRepository<StockReturnItem, UUID> {
    List<StockReturnItem> findAllByClientIdAndStatusTrue(UUID clientId);
    List<StockReturnItem> findAllByClientIdAndStatusFalse(UUID clientId);
    List<StockReturnItem> findAllByClientIdAndStockReturnIdAndStatusTrue(UUID clientId,UUID stockReturnId);
    List<StockReturnItem> findAllByClientIdAndStockReturnIdAndStatusFalse(UUID clientId,UUID stockReturnId);
}
