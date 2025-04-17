package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancelledOrder;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface CancelledOrderRepository extends JpaRepository<CancelledOrder, UUID> {
    CancelledOrder findByOrderingIdAndClientId(UUID orderId,UUID clientId);
    CancelledOrder findByOrderingId(UUID orderId);

    @Modifying
    @Transactional
    @Query("""
        update CancelledOrder oc set 
                oc.updateDate = :updateDate,
                oc.cancellationReasonId = :cancellationReasonId
                where oc.id = :cancelledOrderId and oc.clientId = :clientId and oc.orderingId = :orderId
                """)
    void updateCancelledOrder(
            UUID cancelledOrderId,
            UUID cancellationReasonId,
            UUID clientId,
            UUID orderId,
            OffsetDateTime updateDate
    );
}
