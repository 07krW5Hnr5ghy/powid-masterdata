package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturn;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderReturnRepository extends JpaRepository<OrderReturn,Long> {
    OrderReturn findByOrderId(Long orderId);
}
