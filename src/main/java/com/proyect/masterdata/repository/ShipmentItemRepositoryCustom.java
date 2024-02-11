package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ShipmentItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface ShipmentItemRepositoryCustom {
    Page<ShipmentItem> searchForShipment(
            Long clientId,
            String serial,
            Long warehouseId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
