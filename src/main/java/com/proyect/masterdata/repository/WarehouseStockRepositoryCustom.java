package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.WarehouseStock;

import java.util.List;
import java.util.UUID;

@Repository
public interface WarehouseStockRepositoryCustom {
    Page<WarehouseStock> searchForWarehouseStock(
            UUID clientId,
            String warehouse,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
