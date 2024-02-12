package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.StockTransactionItem;

@Repository
public interface StockTransactionItemRepositoryCustom {
    public Page<StockTransactionItem> searchForStockTransaction(
            Long clientId,
            Long warehouseId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
