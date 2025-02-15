package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransferItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockTransferItemRepositoryCustom {
    Page<StockTransferItem> searchForStockTransferItem(
            UUID clientId,
            List<UUID> stockTransferIds,
            List<UUID> originWarehouseIds,
            List<UUID> destinationWarehouseIds,
            List<UUID> supplierProductIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
