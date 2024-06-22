package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.StockTransactionItem;

import java.util.List;

@Repository
public interface StockTransactionItemRepositoryCustom {
    Page<StockTransactionItem> searchForStockTransactionItem(
            Long clientId,
            Long stockTransactionId,
            Long supplierProductId,
            List<Long> warehouseIds,
            List<Long> stockTransactionTypeIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
