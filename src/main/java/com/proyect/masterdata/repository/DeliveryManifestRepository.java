package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifest;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface DeliveryManifestRepository extends JpaRepository<DeliveryManifest, UUID> {
}
