package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.StockTransactionItem;

@Repository
public interface StockTransactionItemRepository extends JpaRepository<StockTransactionItem, Long> {
    StockTransactionItem findBySerialAndSupplierProductId(String serial, Long supplierProductId);
}
