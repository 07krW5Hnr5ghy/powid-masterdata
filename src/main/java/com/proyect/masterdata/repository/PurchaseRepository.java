package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Purchase;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchaseRepository extends JpaRepository<Purchase, UUID> {
    Purchase findByPurchaseTypeId(UUID purchaseTypeId);
    Purchase findByRef(String serial);
    Purchase findByRefAndPurchaseTypeId(String serial, UUID purchaseTypeId);
    List<Purchase> findByRefIn(List<String> serials);
    Purchase findByPurchaseTypeName(String purchaseTypeName);
    Purchase findByPurchaseTypeNameAndRef(String purchaseTypeName, String serial);
    List<Purchase> findAllByClientId(UUID clientId);
}
