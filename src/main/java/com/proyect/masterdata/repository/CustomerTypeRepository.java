package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CustomerType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface CustomerTypeRepository extends JpaRepository<CustomerType, UUID> {
    CustomerType findByNameAndStatusTrue(String name);
    CustomerType findByNameAndStatusFalse(String name);
    List<CustomerType> findAllByStatusTrue();
}
