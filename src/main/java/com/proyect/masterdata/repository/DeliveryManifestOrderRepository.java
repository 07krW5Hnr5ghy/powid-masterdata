package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifestOrder;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface DeliveryManifestOrderRepository extends JpaRepository<DeliveryManifestOrder, UUID> {
    DeliveryManifestOrder findByDeliveryManifestIdAndOrderIdAndClientId(UUID deliveryManifestId,UUID orderId,UUID clientId);
    @Transactional
    @Modifying
    @Query("UPDATE DeliveryManifestOrder dmo SET " +
            "dmo.delivered = true " +
            "WHERE dmo.deliveryManifestId = :deliveryManifestId "+
            "AND dmo.orderId = :orderId "+
            "AND dmo.clientId = :clientId "
    )
    void setDeliveryManifestOrderDeliveredTrue(
            @Param("orderId") UUID orderId,
            @Param("deliveryManifestId") UUID deliveryManifestId,
            @Param("clientId") UUID clientId
    );
}
