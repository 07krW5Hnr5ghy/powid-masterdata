package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderPaymentReceipt;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderPaymentReceiptRepository extends JpaRepository<OrderPaymentReceipt,Long> {
    List<OrderPaymentReceipt> findAllByOrderId(Long orderId);
}
