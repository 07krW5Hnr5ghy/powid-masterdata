package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Purchase;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PurchaseRepository extends JpaRepository<Purchase,Long> {
    Purchase findByPurchaseTypeId(Long purchaseTypeId);
    Purchase findBySerial(String serial);
    Purchase findBySerialAndPurchaseTypeId(String serial, Long purchaseTypeId);
    List<Purchase> findBySerialIn(List<String> serials);
    Purchase findByPurchaseTypeName(String purchaseTypeName);
    Purchase findByPurchaseTypeNameAndSerial(String purchaseTypeName, String serial);
    List<Purchase> findAllByClientId(Long clientId);
}
