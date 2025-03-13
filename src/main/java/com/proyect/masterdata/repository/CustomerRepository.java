package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Customer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface CustomerRepository extends JpaRepository<Customer, UUID> {
    Customer findByPhone(String phone);
    List<Customer> findAllByClientId(UUID clientId);
    Customer findByIdAndStatusTrue(UUID customerId);
}
