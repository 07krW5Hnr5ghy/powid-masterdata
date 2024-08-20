package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransaction;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockTransactionRepositoryCustom {
    Page<StockTransaction> searchForStockTransaction(
            Long clientId,
            List<String> serials,
            List<Long> warehouseIds,
            List<Long> stockTransactionTypeIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
