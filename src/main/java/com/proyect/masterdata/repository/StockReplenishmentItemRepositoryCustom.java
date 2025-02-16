package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishmentItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockReplenishmentItemRepositoryCustom {
    Page<StockReplenishmentItem> searchForStockReplenishmentItem(
            UUID clientId,
            List<UUID> orderIds,
            List<UUID> productIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
