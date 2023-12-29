package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Warehouse;

@Repository
public interface WarehouseRepositoryCustom {
    public Page<Warehouse> searchForWarehouse(
            String name,
            Long clientId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
