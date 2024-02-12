package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface StockTransactionRepository extends JpaRepository<StockTransaction,Long> {
    StockTransaction findBySerial(String serial);
}
