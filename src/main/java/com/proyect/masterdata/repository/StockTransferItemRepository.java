package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransferItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockTransferItemRepository extends JpaRepository<StockTransferItem,Long> {
    List<StockTransferItem> findAllByClientId(Long clientId);
    List<StockTransferItem> findAllByClientIdAndStockTransferId(Long clientId,Long stockTransferId);
}
