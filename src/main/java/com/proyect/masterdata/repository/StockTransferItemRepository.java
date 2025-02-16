package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransferItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockTransferItemRepository extends JpaRepository<StockTransferItem, UUID> {
    List<StockTransferItem> findAllByClientId(UUID clientId);
    List<StockTransferItem> findAllByClientIdAndStockTransferId(UUID clientId,UUID stockTransferId);
}
