package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ShipmentItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ShipmentItemRepositoryCustom {
    Page<ShipmentItem> searchForShipmentItem(
            Long clientId,
            List<Long> shipmentIds,
            List<Long> purchaseIds,
            List<Long> warehouseIds,
            List<Long> supplierProductIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
