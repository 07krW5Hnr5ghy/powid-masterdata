package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.StockTransactionItem;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockTransactionItemRepository extends JpaRepository<StockTransactionItem, UUID> {
    StockTransactionItem findByStockTransactionIdAndSupplierProductId(UUID stockTransactionId, UUID supplierProductId);
    List<StockTransactionItem> findAllByClientId(UUID clientId);
    List<StockTransactionItem> findAllByClientIdAndStockTransactionId(UUID clientId,UUID transactionId);
}
