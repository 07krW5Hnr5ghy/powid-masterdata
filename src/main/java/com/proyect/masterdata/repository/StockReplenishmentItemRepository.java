package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.StockReplenishmentItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface StockReplenishmentItemRepository extends JpaRepository<StockReplenishmentItem,Long> {
}
