package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.ShipmentItem;

import java.util.List;

public interface ShipmentItemRepository extends JpaRepository<ShipmentItem, Long> {
    ShipmentItem findByShipmentIdAndSupplierProductId(Long shipmentId,Long supplierProductId);
    List<ShipmentItem> findAllByClientId(Long clientId);
    List<ShipmentItem> findAllByClientIdAndShipmentId(Long clientId,Long shipmentId);
}
