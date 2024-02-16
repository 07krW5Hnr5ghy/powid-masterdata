package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishment;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface StockReplenishmentRepositoryCustom {
    Page<StockReplenishment> searchForStockReplenishment(
            Long clientId,
            Long orderId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
