package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Discount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface DiscountRepository extends JpaRepository<Discount,Long> {
    Discount findByName(String name);
    List<Discount> findAllByStatusTrue();
}
