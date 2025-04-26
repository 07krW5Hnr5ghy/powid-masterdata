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
    @Query(value = """
        SELECT product_id AS productId,
               warehouse_id AS warehouseId,
               lot_number AS lotNumber,
               quantity as quantity
        FROM stock.kardex_output as sko
        WHERE sko.client_id = :clientId
          AND sko.delivery_manifest_item_id = :deliveryManifestItemId
        """, nativeQuery = true)
    List<Object[]> selectAllByDeliveryManifestItemIdAndClientId(
            @Param("clientId") UUID clientId,
            @Param("deliveryManifestItemId") UUID deliveryManifestItemId
    );
    @Query(value = """
            SELECT product_id AS productId,
               warehouse_id AS warehouseId,
               lot_number AS lotNumber
        FROM stock.kardex_output
            """,nativeQuery = true)
    List<KardexOutputProjection> selectAll();
}
