package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancelledOrder;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface CancelledOrderRepository extends JpaRepository<CancelledOrder, UUID> {
    CancelledOrder findByOrderingIdAndClientId(UUID orderId,UUID clientId);
    CancelledOrder findByOrderingId(UUID orderId);
}
