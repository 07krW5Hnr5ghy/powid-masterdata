package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifestItem;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.dto.projections.DeliveryManifestItemDTOP;
import com.proyect.masterdata.dto.projections.DeliveryManifestItemProjection;
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
    @Query(value = """
        SELECT dmi.delivery_manifest_item_id AS deliveryManifestItemId,
               usr.username AS username,
               dm.manifest_number AS manifestNumber,
               cu.phone AS phone,
               di.name AS districtName,
               ord.order_number AS orderNumber,
               dmi.product_id AS productId,
               dmi.delivered AS delivered,
               dmi.quantity AS quantity,
               mt.name AS managementType,
               pm.name AS paymentMethod,
               ps.name AS paymentState,
               ord.order_id AS orderId,
               oi.order_item_id AS orderItemId,
               cu.name AS customerName
        FROM logistics.delivery_manifest_items dmi
        JOIN logistics.delivery_manifest dm ON dmi.delivery_manifest_id = dm.delivery_manifest_id
        JOIN ordering.order_item oi ON dmi.order_item_id = oi.order_item_id
        JOIN ordering.order ord ON oi.order_id = ord.order_id
        JOIN ordering.customer cu ON ord.customer_id = cu.customer_id
        JOIN master.management_type mt ON ord.management_type_id = mt.management_type_id
        JOIN master.district di ON cu.district_id = di.district_id
        JOIN master.payment_method pm ON ord.payment_method_id = pm.order_payment_method_id
        JOIN master.payment_state ps ON ord.payment_state_id = ps.payment_state_id
        JOIN management.user_data usr ON dm.user_id = usr.user_id
        WHERE dm.delivery_manifest_id = :deliveryManifestId 
          AND dm.client_id = :clientId
        """, nativeQuery = true)
    List<DeliveryManifestItemProjection> findAllByDeliveryManifestIdAndClientId(
            @Param("deliveryManifestId") UUID deliveryManifestId,
            @Param("clientId") UUID clientId
    );
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
    List<Object[]> countDeliveredOrders(
            @Param("courierId") UUID courierId,
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate
    );

    @Query("""
        SELECT dmi 
        FROM DeliveryManifestItem dmi
        JOIN dmi.deliveryManifest dm
        JOIN dmi.orderItem oi
        WHERE dmi.delivered = TRUE
          AND dmi.collected = FALSE
          AND dm.courierId = :courierId
          AND dmi.registrationDate BETWEEN :startDate AND :endDate
    """)
    List<DeliveryManifestItem> findDeliveredAndUnCollectedOrders(
            @Param("courierId") UUID courierId,
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate
    );

}
