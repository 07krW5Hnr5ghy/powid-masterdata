package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Unit;

@Repository
public interface UnitRepository extends JpaRepository<Unit, UUID> {
    Unit findByNameAndUnitTypeIdAndStatusTrue(String name,UUID unitTypeId);
    Unit findByNameAndUnitTypeIdAndStatusFalse(String name,UUID unitTypeId);
    List<Unit> findByNameInAndStatusTrue(List<String> names);
    List<Unit> findByNameIn(List<String> names);
    List<Unit> findAllByStatusTrue();
    List<Unit> findAllByUnitTypeIdAndStatusTrue(UUID unitTypeId);
}
