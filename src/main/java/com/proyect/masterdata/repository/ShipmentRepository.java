package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Shipment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ShipmentRepository extends JpaRepository<Shipment,Long> {
    Shipment findByShipmentTypeId(Long shipmentTypeId);
    Shipment findBySerial(String serial);
    Shipment findBySerialAndShipmentTypeId(String serial,Long shipmentTypeId);
    List<Shipment> findBySerialIn(List<String> serials);
    Shipment findByShipmentTypeName(String shipmentTypeName);
    Shipment findByShipmentTypeNameAndSerial(String shipmentTypeName,String serial);
    List<Shipment> findAllByClientId(Long clientId);
}
