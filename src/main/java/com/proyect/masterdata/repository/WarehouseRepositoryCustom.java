package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Warehouse;

import java.util.List;
import java.util.UUID;

@Repository
public interface WarehouseRepositoryCustom {
    Page<Warehouse> searchForWarehouse(
            UUID clientId,
            List<String> names,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
