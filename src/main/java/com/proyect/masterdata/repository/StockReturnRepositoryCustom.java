package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturn;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface StockReturnRepositoryCustom {
    public Page<StockReturn> searchForStockReturn(
            Long purchaseId,
            Long clientId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
