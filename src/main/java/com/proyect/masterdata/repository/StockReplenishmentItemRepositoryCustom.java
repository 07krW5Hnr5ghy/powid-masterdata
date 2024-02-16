package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishmentItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface StockReplenishmentItemRepositoryCustom {
    Page<StockReplenishmentItem> searchForStockReplenishmentItem(
            Long clientId,
            Long orderId,
            Long productId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
