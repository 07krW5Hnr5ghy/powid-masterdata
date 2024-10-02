package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturnItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockReturnItemRepositoryCustom {
    Page<StockReturnItem> searchForStockReturnItem(
            Long clientId,
            String serial,
            List<Long> supplierIds,
            String supplierProduct,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
