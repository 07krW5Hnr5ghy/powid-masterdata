package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturnItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface StockReturnItemRepositoryCustom {
    public Page<StockReturnItem> searchForStockReturnItem(
            Long purchaseId,
            Long clientId,
            Long supplierProductId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
