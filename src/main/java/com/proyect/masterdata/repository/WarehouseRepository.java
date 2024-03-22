package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Warehouse;

@Repository
public interface WarehouseRepository extends JpaRepository<Warehouse, Long> {
    Warehouse findByName(String name);
    Warehouse findByNameAndStatusTrue(String name);
    List<Warehouse> findByNameIn(List<String> names);
    List<Warehouse> findAllByClientIdAndStatusTrue(Long clientId);
    List<Warehouse> findAllByClientIdAndStatusFalse(Long clientId);
}
