package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransfer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockTransferRepository extends JpaRepository<StockTransfer, UUID> {
    List<StockTransfer> findAllByClientId(UUID clientId);
    StockTransfer findBySerial(String serial);
    List<StockTransfer> findBySerialIn(List<String> serial);
}
