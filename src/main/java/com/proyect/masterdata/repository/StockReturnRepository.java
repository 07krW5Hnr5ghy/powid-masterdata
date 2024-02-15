package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturn;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface StockReturnRepository extends JpaRepository<StockReturn,Long> {
    StockReturn findByPurchaseId(Long id);
}
