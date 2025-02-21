package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifestItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface DeliveryManifestItemRepository extends JpaRepository<DeliveryManifestItem, UUID> {
}
