package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturnItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockReturnItemRepositoryCustom {
    Page<StockReturnItem> searchForStockReturnItem(
            Long clientId,
            List<Long> stockReturnIds,
            List<Long> shipmentIds,
            List<Long> supplierIds,
            List<Long> supplierProductIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
