package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentMethod;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PaymentMethodRepository extends JpaRepository<PaymentMethod,Long> {

}
