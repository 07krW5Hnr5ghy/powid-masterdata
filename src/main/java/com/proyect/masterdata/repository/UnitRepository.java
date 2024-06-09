package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Unit;

@Repository
public interface UnitRepository extends JpaRepository<Unit, Long> {
    Unit findByNameAndStatusTrue(String name);
    Unit findByNameAndStatusFalse(String name);
    List<Unit> findByNameInAndStatusTrue(List<String> names);
    List<Unit> findAllByStatusTrue();
    List<Unit> findAllByUnitTypeIdAndStatusTrue(Long unitTypeId);
}
