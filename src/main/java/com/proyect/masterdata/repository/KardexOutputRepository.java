package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.KardexOutput;
import com.proyect.masterdata.dto.projections.KardexOutputProjection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface KardexOutputRepository extends JpaRepository<KardexOutput, UUID> {
    @Query("""
            SELECT ko 
            FROM KardexOutput ko 
            WHERE ko.clientId = :clientId AND
            ko.deliveryManifestItemId = :deliveryManifestItemId
            """)
    List<KardexOutputProjection> findAllByDeliveryManifestItemIdAndClientId(
            @Param("clientId") UUID clientId,
            @Param("deliveryManifestItemId") UUID deliveryManifestItemId
    );
}
