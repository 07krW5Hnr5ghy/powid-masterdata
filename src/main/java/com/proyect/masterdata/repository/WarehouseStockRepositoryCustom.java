package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.WarehouseStock;

import java.util.List;

@Repository
public interface WarehouseStockRepositoryCustom {
    Page<WarehouseStock> searchForWarehouseStock(
            Long clientId,
            List<Long> warehouseIds,
            String serial,
            String productSku,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
