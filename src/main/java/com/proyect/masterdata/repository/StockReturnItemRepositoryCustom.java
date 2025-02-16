package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturnItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockReturnItemRepositoryCustom {
    Page<StockReturnItem> searchForStockReturnItem(
            UUID clientId,
            String serial,
            List<UUID> supplierIds,
            String supplierProduct,
            String product,
            String model,
            String color,
            String size,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
