package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.WarehouseStock;

import java.util.List;

@Repository
public interface WarehouseStockRepository extends JpaRepository<WarehouseStock, Long> {
    WarehouseStock findByWarehouseIdAndSupplierProductId(Long warehouseId, Long supplierProductId);
    List<WarehouseStock> findAllByWarehouseIdAndSupplierProductId(Long warehouseId, Long supplierProductId);
    List<WarehouseStock> findAllBySupplierProductId(Long supplierProductId);
    List<WarehouseStock> findAllByClientId(Long clientId);
    List<WarehouseStock> findAllByClientIdAndWarehouseId(Long clientId,Long warehouseId);

}
