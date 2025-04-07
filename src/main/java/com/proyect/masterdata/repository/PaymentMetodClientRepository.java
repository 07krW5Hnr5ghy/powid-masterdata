package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentMetodClient;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface PaymentMetodClientRepository extends JpaRepository<PaymentMetodClient, UUID> {
    PaymentMetodClient findByAccountDetailAndStatusTrue(String name);
    List<PaymentMetodClient> findByClientId(UUID clientId);
}
