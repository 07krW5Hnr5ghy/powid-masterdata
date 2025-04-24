package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.KardexBalance;
import com.proyect.masterdata.domain.KardexInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface KardexBalanceRepository extends JpaRepository<KardexBalance, UUID> {
    KardexBalance findByProductIdAndClientIdAndLotNumber(UUID productId,UUID clientId,Long lotNumber);
    @Query("""
            SELECT kb 
            FROM KardexBalance kb 
            WHERE kb.clientId = :clientId AND
            kb.productId = :productId AND
            kb.warehouseId = :warehouseId AND
            kb.remainingQuantity > 0
            ORDER BY kb.registrationDate ASC
            """)
    List<KardexBalance> findAllByClientIdAndProductIdAndWarehouseIdWithStock(
            @Param("clientId") UUID clientId,
            @Param("productId") UUID productId,
            @Param("warehouseId") UUID warehouseId
    );
    @Query("""
            SELECT kb 
            FROM KardexBalance kb 
            WHERE kb.clientId = :clientId AND
            kb.productId = :productId AND
            kb.warehouseId = :warehouseId AND
            kb.lotNumber = :lotNumber
            """)
    KardexBalance findAllByClientIdAndProductIdAndWarehouseIdWithoutSockCheck(
            @Param("clientId") UUID clientId,
            @Param("productId") UUID productId,
            @Param("warehouseId") UUID warehouseId,
            @Param("lotNumber") Long lotNumber
    );
    Long countByClientIdAndProductId(UUID clientId,UUID productId);
    @Query("""
            SELECT kb 
            FROM KardexBalance kb 
            WHERE kb.clientId = :clientId AND
            kb.productId = :productId AND
            kb.warehouseId = :warehouseId AND
            kb.lotNumber = :lotNumber AND
            kb.remainingQuantity > 0
            ORDER BY kb.registrationDate ASC
            """)
    KardexBalance findByClientIdAndProductIdAndLotNumberAndLotNumberWithStock(
            @Param("clientId") UUID clientId,
            @Param("productId") UUID productId,
            @Param("warehouseId") UUID warehouseId,
            @Param("lotNumber") Long lotNumber
    );
}
