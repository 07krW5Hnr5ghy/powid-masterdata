package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.WarehouseStock;

import java.util.List;
import java.util.UUID;

@Repository
public interface WarehouseStockRepository extends JpaRepository<WarehouseStock, UUID> {
    WarehouseStock findByWarehouseIdAndProductId(UUID warehouseId, UUID productId);
    List<WarehouseStock> findAllByWarehouseIdAndProductId(UUID warehouseId, UUID productId);
    List<WarehouseStock> findAllByProductId(UUID productId);
    List<WarehouseStock> findAllByClientId(UUID clientId);
    List<WarehouseStock> findAllByClientIdAndWarehouseId(UUID clientId,UUID warehouseId);
    List<WarehouseStock> findByClientIdAndWarehouseIdAndProductId(UUID clientId,UUID warehouseId,UUID productId);
    WarehouseStock findProductByProductId(UUID productId);
}
