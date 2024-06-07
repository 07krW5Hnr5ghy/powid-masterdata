package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseDocument;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PurchaseDocumentRepository extends JpaRepository<PurchaseDocument,Long> {
    PurchaseDocument findByNameAndStatusTrue(String name);
    PurchaseDocument findByNameAndStatusFalse(String name);
    PurchaseDocument findByName(String name);
    List<PurchaseDocument> findAllByStatusTrue();
}
