package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.SupplierProduct;

@Repository
public interface SupplierProductRepository extends JpaRepository<SupplierProduct, Long> {
    SupplierProduct findBySerial(String serial);

    SupplierProduct findBySerialAndStatusTrue(String serial);

    List<SupplierProduct> findBySerialIn(List<String> serials);
}
