package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface DeliveryManifestRepository extends JpaRepository<DeliveryManifest, UUID> {
    Long countByClientId(UUID clientId);
    DeliveryManifest findByUserId(UUID userId);
    DeliveryManifest findByCourierId(UUID courierId);

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
}
