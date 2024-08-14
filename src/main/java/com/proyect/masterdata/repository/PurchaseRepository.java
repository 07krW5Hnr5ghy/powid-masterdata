package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Purchase;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PurchaseRepository extends JpaRepository<Purchase,Long> {
    Purchase findByPurchaseTypeId(Long shipmentTypeId);
    Purchase findBySerial(String serial);
    Purchase findBySerialAndPurchaseTypeId(String serial, Long shipmentTypeId);
    List<Purchase> findBySerialIn(List<String> serials);
    Purchase findByPurchaseTypeName(String shipmentTypeName);
    Purchase findByPurchaseTypeNameAndSerial(String shipmentTypeName, String serial);
    List<Purchase> findAllByClientId(Long clientId);
}
