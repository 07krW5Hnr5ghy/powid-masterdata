package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ShipmentType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ShipmentTypeRepository extends JpaRepository<ShipmentType,Long> {
    ShipmentType findByName(String name);
}
