package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchasePaymentType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchasePaymentTypeRepository extends JpaRepository<PurchasePaymentType,UUID> {
    PurchasePaymentType findByName(String name);
    PurchasePaymentType findByNameAndStatusTrue(String name);
    List<PurchasePaymentType> findAllByStatusTrue();
}
