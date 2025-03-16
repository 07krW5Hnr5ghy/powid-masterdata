package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.StockTransactionItem;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockTransactionItemRepositoryCustom {
    Page<StockTransactionItem> searchForStockTransactionItem(
            UUID clientId,
            String serial,
            String product,
            List<UUID> warehouseIds,
            List<UUID> stockTransactionTypeIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
