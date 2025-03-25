package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifestItem;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.dto.projections.DeliveryManifestItemDTOP;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface    DeliveryManifestItemRepository extends JpaRepository<DeliveryManifestItem, UUID> {
    List<DeliveryManifestItem> findAllById(UUID deliveryManifestId);
    DeliveryManifestItem findByOrderItemIdAndProductIdAndDeliveredTrue(UUID orderItemId,UUID productId);
    List<DeliveryManifestItemDTOP> findAllByDeliveryManifestId(UUID deliveryManifestId);

    @Query("""
    SELECT dmi.deliveryManifest.id, oi.orderId, 
           COUNT(CASE WHEN dmi.delivered = true THEN 1 END) AS deliveredCount
    FROM DeliveryManifestItem dmi
    JOIN dmi.deliveryManifest dm
    JOIN dmi.orderItem oi
    WHERE dm.courier.id = :courierId
    AND dmi.registrationDate BETWEEN :startDate AND :endDate
    GROUP BY dmi.deliveryManifest.id, oi.orderId
    """)
    List<Object[]> countDeliveredAndCollectedOrders(
            @Param("courierId") UUID courierId,
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate
    );

}
