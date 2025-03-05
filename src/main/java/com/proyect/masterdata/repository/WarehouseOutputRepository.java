package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.WarehouseOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface WarehouseOutputRepository extends JpaRepository<WarehouseOutput, UUID> {
    Long countByClientId(UUID clientId);
}
