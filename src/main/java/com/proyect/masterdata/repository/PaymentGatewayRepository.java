package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentGateway;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PaymentGatewayRepository extends JpaRepository<PaymentGateway,Long> {
    PaymentGateway findByName(String name);
    PaymentGateway findByNameAndStatusTrue(String name);
}
