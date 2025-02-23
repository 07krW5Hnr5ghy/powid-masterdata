package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifestStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface DeliveryManifestStatusRepository extends JpaRepository<DeliveryManifestStatus, UUID> {
    DeliveryManifestStatus findByName(String name);
    List<DeliveryManifestStatus> findAllByStatusTrue();
}
