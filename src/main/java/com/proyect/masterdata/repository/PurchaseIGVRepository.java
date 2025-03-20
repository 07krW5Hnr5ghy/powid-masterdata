package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseIGV;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchaseIGVRepository extends JpaRepository<PurchaseIGV, UUID> {
    PurchaseIGV findByName(String name);
    PurchaseIGV findByNameAndStatusTrue(String name);
    List<PurchaseIGV> findAllByStatusTrue();
}
