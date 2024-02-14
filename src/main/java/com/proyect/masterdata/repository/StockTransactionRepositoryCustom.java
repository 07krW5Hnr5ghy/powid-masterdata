package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransaction;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface StockTransactionRepositoryCustom {
    Page<StockTransaction> searchForStockTransaction(
            Long clientId,
            String serial,
            Long warehouseId,
            Long stockTransactionTypeId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
