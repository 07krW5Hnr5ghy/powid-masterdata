package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturn;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StockReturnRepository extends JpaRepository<StockReturn, UUID> {
    List<StockReturn> findAllByClientIdAndStatusTrue(UUID clientId);
    List<StockReturn> findAllByClientIdAndStatusFalse(UUID clientId);
    List<StockReturn> findAllByClientId(UUID clientId);
    StockReturn findBySerial(String serial);
    List<StockReturn> findBySerialIn(List<String> serials);
}
