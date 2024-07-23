package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Warehouse;

import java.util.List;

@Repository
public interface WarehouseRepositoryCustom {
    Page<Warehouse> searchForWarehouse(
            Long clientId,
            List<String> names,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
