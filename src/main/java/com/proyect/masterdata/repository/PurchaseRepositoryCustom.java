package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Purchase;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PurchaseRepositoryCustom {
    Page<Purchase> searchForShipment(
            Long clientId,
            List<String> serials,
            List<Long> warehouseIds,
            List<Long> shipmentTypeIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
