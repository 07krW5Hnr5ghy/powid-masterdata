package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Shipment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ShipmentRepository extends JpaRepository<Shipment,Long> {
    Shipment findByPurchaseSerialAndShipmentTypeId(String serial,Long shipmentTypeId);
    Shipment findBySerial(String serial);
    Shipment findByPurchaseIdAndShipmentTypeName(Long purchaseId,String shipmentTypeName);
    List<Shipment> findAllByClientId(Long clientId);
}
