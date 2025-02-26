package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderLogRepository extends JpaRepository<OrderLog,UUID> {
    List<OrderLog> findAllByOrderId(UUID orderId);
}
