package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.WarehouseOutputItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface WarehouseOutputItemRepository extends JpaRepository<WarehouseOutputItem, UUID> {
    WarehouseOutputItem findByProductIdAndWarehouseOutputId(UUID productId,UUID warehouseOutputId);
}
