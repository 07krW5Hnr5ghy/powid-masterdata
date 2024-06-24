package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Purchase;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PurchaseRepository extends JpaRepository<Purchase,Long> {
    Purchase findBySerial(String serial);
    Purchase findBySerialAndStatusTrue(String serial);
    List<Purchase> findAllByClientIdAndStatusTrue(Long clientId);
    List<Purchase> findAllByClientIdAndStatusFalse(Long clientId);
    List<Purchase> findAllByClientId(Long clientId);
}
