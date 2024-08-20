package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishment;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockReplenishmentRepositoryCustom {
    Page<StockReplenishment> searchForStockReplenishment(
            Long clientId,
            List<Long> orderIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
