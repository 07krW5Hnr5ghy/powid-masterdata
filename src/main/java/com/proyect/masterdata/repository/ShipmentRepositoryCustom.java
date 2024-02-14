package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Shipment;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface ShipmentRepositoryCustom {
    Page<Shipment> searchForShipment(
            Long clientId,
            String purchaseSerial,
            Long warehouseId,
            Long shipmentTypeId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
