package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderPaymentReceipt;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderPaymentReceiptRepository extends JpaRepository<OrderPaymentReceipt,Long> {
    List<OrderPaymentReceipt> findAllByOrderId(UUID orderId);
    List<OrderPaymentReceipt> findAllByOrderIdIn(List<UUID> orderIds);
}
