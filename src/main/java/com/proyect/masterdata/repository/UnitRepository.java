package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Unit;

@Repository
public interface UnitRepository extends JpaRepository<Unit, UUID> {
    Unit findByNameAndUnitTypeIdAndClientIdAndStatusTrue(String name,UUID unitTypeId,UUID clientId);
    Unit findByNameAndUnitTypeIdAndClientIdAndStatusFalse(String name,UUID unitTypeId,UUID clientId);
    List<Unit> findByClientIdAndNameInAndStatusTrue(UUID clientId,List<String> names);
    List<Unit> findByClientIdAndNameIn(UUID clientId,List<String> names);
    List<Unit> findAllByClientIdAndStatusTrue(UUID clientId);
    List<Unit> findAllByUnitTypeIdAndClientIdAndStatusTrue(UUID clientId,UUID unitTypeId);
    List<Unit> findAllByClientId(UUID clientId);
}
