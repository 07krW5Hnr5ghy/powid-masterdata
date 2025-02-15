package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturn;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderReturnRepository extends JpaRepository<OrderReturn, UUID> {
    OrderReturn findByOrderId(UUID orderId);
    OrderReturn findByOrderIdAndClientId(UUID orderId,UUID clientId);
    List<OrderReturn> findAllByClientIdAndStatusTrue(UUID clientId);
    List<OrderReturn> findAllByClientId(UUID clientId);
}
