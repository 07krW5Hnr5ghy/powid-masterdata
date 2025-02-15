package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentGateway;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface PaymentGatewayRepository extends JpaRepository<PaymentGateway, UUID> {
    PaymentGateway findByName(String name);
    PaymentGateway findByNameAndStatusTrue(String name);
    PaymentGateway findByNameAndStatusFalse(String name);
}
