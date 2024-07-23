package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ShipmentDocument;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ShipmentDocumentRepository extends JpaRepository<ShipmentDocument,Long> {
    ShipmentDocument findByNameAndStatusTrue(String name);
    ShipmentDocument findByNameAndStatusFalse(String name);
    ShipmentDocument findByName(String name);
    List<ShipmentDocument> findByNameIn(List<String> names);
    List<ShipmentDocument> findAllByStatusTrue();
}
