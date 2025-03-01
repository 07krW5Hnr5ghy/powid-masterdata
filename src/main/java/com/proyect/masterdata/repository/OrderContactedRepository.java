package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderContacted;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface OrderContactedRepository extends JpaRepository<OrderContacted, UUID> {
    OrderContacted findByOrderId(UUID orderId);
}
