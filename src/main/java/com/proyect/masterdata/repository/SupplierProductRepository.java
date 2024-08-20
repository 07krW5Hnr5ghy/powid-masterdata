package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.SupplierProduct;

@Repository
public interface SupplierProductRepository extends JpaRepository<SupplierProduct, Long> {
    SupplierProduct findBySerial(String serial);
    SupplierProduct findBySerialAndStatusTrue(String serial);
    SupplierProduct findBySerialAndStatusFalse(String serial);
    List<SupplierProduct> findBySerialIn(List<String> serials);
    List<SupplierProduct> findAllByClientId(Long id);
    List<SupplierProduct> findAllByProductIdAndStatusTrue(Long id);
    List<SupplierProduct> findAllByClientIdAndStatusTrue(Long clientId);
    List<SupplierProduct> findAllByClientIdAndStatusFalse(Long clientId);
    List<SupplierProduct> findAllByClientIdAndSupplierIdAndStatusTrue(Long clientId,Long supplierId);
    List<SupplierProduct> findAllByClientIdAndSupplierIdAndStatusFalse(Long clientId,Long supplierId);
    List<SupplierProduct> findAllByClientIdAndProductIdAndStatusTrue(Long clientId,Long productId);
}
