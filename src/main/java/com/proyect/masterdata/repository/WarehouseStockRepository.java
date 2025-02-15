package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.WarehouseStock;

import java.util.List;
import java.util.UUID;

@Repository
public interface WarehouseStockRepository extends JpaRepository<WarehouseStock, UUID> {
    WarehouseStock findByWarehouseIdAndSupplierProductId(UUID warehouseId, UUID supplierProductId);
    List<WarehouseStock> findAllByWarehouseIdAndSupplierProductId(UUID warehouseId, UUID supplierProductId);
    List<WarehouseStock> findAllBySupplierProductId(UUID supplierProductId);
    List<WarehouseStock> findAllByClientId(UUID clientId);
    List<WarehouseStock> findAllByClientIdAndWarehouseId(UUID clientId,UUID warehouseId);
    List<WarehouseStock> findByClientIdAndWarehouseIdAndSupplierProduct_Supplier_Id(UUID clientId,UUID warehouseId,UUID supplierId);
}
