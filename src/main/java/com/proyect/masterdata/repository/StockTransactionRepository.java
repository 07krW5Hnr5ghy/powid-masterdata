package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.StockTransaction;

@Repository
public interface StockTransactionRepository extends JpaRepository<StockTransaction, Long> {

}
