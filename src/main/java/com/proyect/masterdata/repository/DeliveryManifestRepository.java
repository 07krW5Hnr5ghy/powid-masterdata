package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifest;
import com.proyect.masterdata.domain.User;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface DeliveryManifestRepository extends JpaRepository<DeliveryManifest, UUID> {
    Long countByClientId(UUID clientId);

    DeliveryManifest findByUserId(UUID userId);

    DeliveryManifest findByCourierId(UUID courierId);
    DeliveryManifest findByCourierIdAndOpenTrue(UUID courierId);
    @Query("""
        SELECT dm 
        FROM DeliveryManifest dm
        WHERE dm.courierId = :courierId
        ORDER BY dm.registrationDate DESC
    """)
    List<DeliveryManifest> findLatestByCourier(
            @Param("courierId") UUID courierId,
            Pageable pageable
    );

//    @Modifying
//    @Transactional
//    @Query("UPDATE OrderItem o SET " +
//            "o.selectOrderStatus = :selectOrderStatus," +
//            "o.updateDate = :updateDate " +
//            "WHERE o.userId = :userId AND o.orderId = :orderId AND o.id = :orderItemId")
//    void selectPreparedOrdetItem (
//            @Param("orderId") UUID orderId,
//            @Param("orderItemId") UUID orderItemId,
//            @Param("userId") UUID userId,
//            @Param("updateDate") OffsetDateTime updateDate,
//            @Param("selectOrderStatus") Boolean selectOrderStatus
//    );

    @Modifying
    @Transactional
    @Query("""
        update DeliveryManifest dm set 
            dm.open = :open,
            dm.updateDate = :updateDate,
            dm.userId = :userId
                where dm.id = :deliveryManifestId
    """)
    void closeDeliveriManifest(UUID deliveryManifestId, UUID userId, boolean open, OffsetDateTime updateDate);
}



















