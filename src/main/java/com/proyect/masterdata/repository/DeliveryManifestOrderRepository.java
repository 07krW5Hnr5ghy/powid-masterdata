package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifestOrder;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface DeliveryManifestOrderRepository extends JpaRepository<DeliveryManifestOrder, UUID> {
    DeliveryManifestOrder findByDeliveryManifestIdAndOrderIdAndClientId(UUID deliveryManifestId,UUID orderId,UUID clientId);
}
