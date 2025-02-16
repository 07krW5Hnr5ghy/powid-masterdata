package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Purchase;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchaseRepository extends JpaRepository<Purchase, UUID> {
    Purchase findByPurchaseTypeId(UUID purchaseTypeId);
    Purchase findBySerial(String serial);
    Purchase findBySerialAndPurchaseTypeId(String serial, UUID purchaseTypeId);
    List<Purchase> findBySerialIn(List<String> serials);
    Purchase findByPurchaseTypeName(String purchaseTypeName);
    Purchase findByPurchaseTypeNameAndSerial(String purchaseTypeName, String serial);
    List<Purchase> findAllByClientId(UUID clientId);
}
