package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Shipment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ShipmentRepository extends JpaRepository<Shipment,Long> {
    Shipment findBySerial(String serial);
}
