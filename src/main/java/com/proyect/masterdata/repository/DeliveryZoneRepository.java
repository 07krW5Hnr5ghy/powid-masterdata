package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryZone;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface DeliveryZoneRepository extends JpaRepository<DeliveryZone, UUID> {
    List<DeliveryZone> findAllByStatusTrueAndClientId(UUID clientId);
    List<DeliveryZone> findAllByStatusFalseAndClientId(UUID clientId);
    DeliveryZone findByIdAndClientIdAndStatusTrue(UUID id,UUID clientId);
    DeliveryZone findByNameAndClientIdAndStatusTrue(String name,UUID clientId);
    DeliveryZone findByNameAndClientIdAndStatusFalse(String name,UUID clientId);
    DeliveryZone findByNameAndClientId(String name,UUID clientId);
    List<DeliveryZone> findByClientIdAndNameIn(UUID clientId,List<String> names);
    List<DeliveryZone> findAllByClientId(UUID clientId);
}
