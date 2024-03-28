package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.StockTransactionItem;

import java.util.List;

@Repository
public interface StockTransactionItemRepository extends JpaRepository<StockTransactionItem, Long> {
    StockTransactionItem findByStockTransactionIdAndSupplierProductId(Long stockTransactionId, Long supplierProductId);
    List<StockTransactionItem> findAllByClientId(Long clientId);
    List<StockTransactionItem> findAllByClientIdAndStockTransactionId(Long clientId,Long transactionId);
}
