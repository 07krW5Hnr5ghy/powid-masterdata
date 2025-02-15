package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseDocument;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchaseDocumentRepository extends JpaRepository<PurchaseDocument, UUID> {
    PurchaseDocument findByNameAndStatusTrue(String name);
    PurchaseDocument findByNameAndStatusFalse(String name);
    PurchaseDocument findByName(String name);
    List<PurchaseDocument> findByNameIn(List<String> names);
    List<PurchaseDocument> findAllByStatusTrue();
}
