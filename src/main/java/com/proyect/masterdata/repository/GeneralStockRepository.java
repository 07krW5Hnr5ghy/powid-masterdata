package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.GeneralStock;

import java.util.List;
import java.util.UUID;

public interface GeneralStockRepository extends JpaRepository<GeneralStock, UUID> {
    GeneralStock findByClientIdAndSupplierProductId(UUID clientId, UUID supplierProduct);
    List<GeneralStock> findAllByClientId(UUID clientId);
}
