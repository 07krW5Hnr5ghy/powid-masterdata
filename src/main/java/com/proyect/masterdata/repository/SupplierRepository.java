package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Supplier;

@Repository
public interface SupplierRepository extends JpaRepository<Supplier, Long> {
    Supplier findByRucAndStatusTrue(String ruc);

    List<Supplier> findByRucInAndStatusTrue(List<String> rucList);
}
