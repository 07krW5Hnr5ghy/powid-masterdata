package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchasePaymentMethod;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchasePaymentMethodRepository extends JpaRepository<PurchasePaymentMethod, UUID> {
    List<PurchasePaymentMethod> findAllByStatusTrue();
    List<PurchasePaymentMethod> findAllByStatusFalse();
    PurchasePaymentMethod findByIdAndStatusTrue(UUID id);
    PurchasePaymentMethod findByNameAndStatusTrue(String name);
    List<PurchasePaymentMethod> findByNameIn(List<String> names);
    PurchasePaymentMethod findByName(String name);
}
