package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentState;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PaymentStateRepository extends JpaRepository<PaymentState,Long> {
    PaymentState findByName(String name);
}
