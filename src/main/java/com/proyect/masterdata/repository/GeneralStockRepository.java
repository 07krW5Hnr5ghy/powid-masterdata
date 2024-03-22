package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.GeneralStock;

import java.util.List;

public interface GeneralStockRepository extends JpaRepository<GeneralStock, Long> {
    GeneralStock findByClientIdAndSupplierProductId(Long clientId, Long supplierProduct);
    List<GeneralStock> findAllByClientIdAndStatusTrue(Long clientId);
    List<GeneralStock> findAllByClientIdAndStatusFalse(Long clientId);
}
