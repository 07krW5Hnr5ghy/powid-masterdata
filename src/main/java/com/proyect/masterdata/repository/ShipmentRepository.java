package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.Shipment;

public interface ShipmentRepository extends JpaRepository<Shipment, Long> {
    Shipment findBySerial(String serial);
}
