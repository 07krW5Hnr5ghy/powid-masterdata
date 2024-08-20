package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishmentItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockReplenishmentItemRepositoryCustom {
    Page<StockReplenishmentItem> searchForStockReplenishmentItem(
            Long clientId,
            List<Long> orderIds,
            List<Long> productIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
