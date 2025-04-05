package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.GeneralStock;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;
@Repository
public interface GeneralStockRepository extends JpaRepository<GeneralStock, UUID> {
    List<GeneralStock> findAllByClientId(UUID clientId);
    GeneralStock findByClientIdAndProductId(UUID clientId,UUID productId);
}
