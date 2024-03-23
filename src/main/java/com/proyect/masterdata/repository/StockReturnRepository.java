package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturn;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockReturnRepository extends JpaRepository<StockReturn,Long> {
    StockReturn findByPurchaseId(Long id);
    List<StockReturn> findAllByClientIdAndStatusTrue(Long clientId);
    List<StockReturn> findAllByClientIdAndStatusFalse(Long clientId);
}
