package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.StockTransaction;

@Repository
public interface StockTransactionRepositoryCustom {
    public Page<StockTransaction> searchForStockTransaction(
            Long clientId,
            Long warehouseId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
