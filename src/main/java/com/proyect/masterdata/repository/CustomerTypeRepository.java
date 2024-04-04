package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CustomerType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CustomerTypeRepository extends JpaRepository<CustomerType,Long> {
    CustomerType findByNameAndStatusTrue(String name);
    List<CustomerType> findAllByStatusTrue();
}
