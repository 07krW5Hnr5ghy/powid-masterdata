package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockTransactionRepository extends JpaRepository<StockTransaction, UUID> {
    StockTransaction findBySerial(String serial);
    List<StockTransaction> findBySerialIn(List<String> serials);
    List<StockTransaction> findAllByClientId(UUID clientId);
    StockTransaction findByWarehouseId(UUID warehouseId);
}
