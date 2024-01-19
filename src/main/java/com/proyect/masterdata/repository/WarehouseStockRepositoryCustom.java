package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.WarehouseStock;

@Repository
public interface WarehouseStockRepositoryCustom {
    Page<WarehouseStock> searchForWarehouseStock(
            Long clientId,
            Long warehouseId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
