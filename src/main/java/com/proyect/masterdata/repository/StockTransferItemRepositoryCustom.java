package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransferItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockTransferItemRepositoryCustom {
    Page<StockTransferItem> searchForStockTransferItem(
            Long clientId,
            List<Long> stockTransferIds,
            List<Long> originWarehouseIds,
            List<Long> destinationWarehouseIds,
            List<Long> supplierProductIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
