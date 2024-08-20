package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SupplierType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface SupplierTypeRepository extends JpaRepository<SupplierType,Long> {
    SupplierType findByNameAndStatusTrue(String name);
    SupplierType findByNameAndStatusFalse(String name);
    List<SupplierType> findByNameIn(List<String> names);
    List<SupplierType> findAllByStatusTrue();
}
