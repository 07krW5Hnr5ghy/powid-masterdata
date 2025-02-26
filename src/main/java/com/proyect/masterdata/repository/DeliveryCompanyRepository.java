package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryCompany;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface DeliveryCompanyRepository extends JpaRepository<DeliveryCompany, UUID> {
    DeliveryCompany findByName(String name);
    List<DeliveryCompany> findAllByStatusTrue();
}
