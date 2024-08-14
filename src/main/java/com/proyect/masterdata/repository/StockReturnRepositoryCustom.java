package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturn;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockReturnRepositoryCustom {
    Page<StockReturn> searchForStockReturnItem(
            Long clientId,
            List<String> serials,
            List<Long> purchaseIds,
            List<Long> supplierIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
