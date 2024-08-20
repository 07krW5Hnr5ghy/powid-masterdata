package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReturn;
import com.proyect.masterdata.domain.StockReturnItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockReturnItemRepository extends JpaRepository<StockReturnItem,Long> {
    List<StockReturnItem> findAllByClientIdAndStatusTrue(Long clientId);
    List<StockReturnItem> findAllByClientIdAndStatusFalse(Long clientId);
    List<StockReturnItem> findAllByClientIdAndStockReturnIdAndStatusTrue(Long clientId,Long stockReturnId);
    List<StockReturnItem> findAllByClientIdAndStockReturnIdAndStatusFalse(Long clientId,Long stockReturnId);
}
