package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransferItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface StockTransferItemRepositoryCustom {
    Page<StockTransferItem> searchForStockTransferItem(
            Long clientId,
            Long stockTransferId,
            Long supplierProductId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
