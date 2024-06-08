package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ShipmentType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ShipmentTypeRepository extends JpaRepository<ShipmentType,Long> {
    ShipmentType findByName(String name);
    ShipmentType findByNameAndStatusTrue(String name);
    ShipmentType findByNameAndStatusFalse(String name);
    List<ShipmentType> findAllByStatusTrue();
}
