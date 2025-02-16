package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransaction;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockTransactionRepositoryCustom {
    Page<StockTransaction> searchForStockTransaction(
            UUID clientId,
            List<String> serials,
            List<UUID> warehouseIds,
            List<UUID> stockTransactionTypeIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
