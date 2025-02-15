package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Discount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface DiscountRepository extends JpaRepository<Discount, UUID> {
    Discount findByName(String name);
    Discount findByNameAndStatusTrue(String name);
    List<Discount> findAllByStatusTrue();
}
