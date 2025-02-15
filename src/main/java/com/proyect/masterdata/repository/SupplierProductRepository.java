package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.SupplierProduct;

@Repository
public interface SupplierProductRepository extends JpaRepository<SupplierProduct, UUID> {
    SupplierProduct findBySerial(String serial);
    SupplierProduct findBySerialAndStatusTrue(String serial);
    SupplierProduct findBySerialAndStatusFalse(String serial);
    List<SupplierProduct> findBySerialIn(List<String> serials);
    List<SupplierProduct> findAllByClientId(UUID id);
    List<SupplierProduct> findAllByProductIdAndStatusTrue(UUID id);
    List<SupplierProduct> findAllByClientIdAndStatusTrue(UUID clientId);
    List<SupplierProduct> findAllByClientIdAndStatusFalse(UUID clientId);
    List<SupplierProduct> findAllByClientIdAndSupplierIdAndStatusTrue(UUID clientId,UUID supplierId);
    List<SupplierProduct> findAllByClientIdAndSupplierIdAndStatusFalse(UUID clientId,UUID supplierId);
    List<SupplierProduct> findAllByClientIdAndProductIdAndStatusTrue(UUID clientId,UUID productId);
}
