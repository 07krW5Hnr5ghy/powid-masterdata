package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PaymentTypeRepository extends JpaRepository<PaymentType, Long> {
    boolean existsByType(String type);

    List<PaymentType> findByTypeIn(List<String> typeList);

    PaymentType findByType(String type);
}
