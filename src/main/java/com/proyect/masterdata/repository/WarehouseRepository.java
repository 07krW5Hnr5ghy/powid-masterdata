package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Warehouse;

@Repository
public interface WarehouseRepository extends JpaRepository<Warehouse, UUID> {
    Warehouse findByName(String name);
    Warehouse findByNameAndStatusTrue(String name);
    List<Warehouse> findByNameIn(List<String> names);
    List<Warehouse> findByIdIn(List<UUID> ids);
    List<Warehouse> findAllByClientIdAndStatusTrue(UUID clientId);
    List<Warehouse> findAllByClientIdAndStatusFalse(UUID clientId);
    List<Warehouse> findAllByClientId(UUID clientId);
    Warehouse findByClientIdAndNameAndStatusTrue(UUID clientId,String name);
}
