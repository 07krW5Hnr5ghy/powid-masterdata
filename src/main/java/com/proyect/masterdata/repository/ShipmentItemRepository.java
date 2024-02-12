package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.ShipmentItem;

public interface ShipmentItemRepository extends JpaRepository<ShipmentItem, Long> {
    ShipmentItem findByShipmentIdAndSupplierProductId(Long shipmentId,Long supplierProductId);
}
