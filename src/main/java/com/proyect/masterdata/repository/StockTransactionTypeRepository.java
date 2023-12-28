package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.StockTransactionType;

@Repository
public interface StockTransactionTypeRepository extends JpaRepository<StockTransactionType, Long> {
    StockTransactionType findByName(String name);

    StockTransactionType findByNameAndStatusTrue(String name);

    List<StockTransactionType> findByNameIn(List<String> names);

    List<StockTransactionType> findAllByStatusTrue();
}
