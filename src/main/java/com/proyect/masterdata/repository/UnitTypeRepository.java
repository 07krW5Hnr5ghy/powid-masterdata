package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.UnitType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface UnitTypeRepository extends JpaRepository<UnitType, UUID> {
    UnitType findByName(String name);
    UnitType findByNameAndStatusTrue(String name);
    UnitType findByNameAndStatusFalse(String name);
    List<UnitType> findAllByStatusTrue();
}
