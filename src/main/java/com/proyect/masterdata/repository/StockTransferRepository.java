package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockTransfer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StockTransferRepository extends JpaRepository<StockTransfer,Long> {
    List<StockTransfer> findAllByClientId(Long clientId);
}
