package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseDiscount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchaseDiscountRepository extends JpaRepository<PurchaseDiscount, UUID> {
    PurchaseDiscount findByName(String name);
    PurchaseDiscount findByNameAndStatusTrue(String name);
    List<PurchaseDiscount> findAllByStatusTrue();
}
