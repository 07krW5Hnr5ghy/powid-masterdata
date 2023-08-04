package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.PaymentMethod;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface PaymentMethodRepository extends JpaRepository<PaymentMethod,Long> {
    PaymentMethod findByName(String name);
}
