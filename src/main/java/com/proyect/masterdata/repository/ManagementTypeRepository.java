package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ManagementType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ManagementTypeRepository extends JpaRepository<ManagementType,Long> {
    ManagementType findByNameAndStatusTrue(String name);
    ManagementType findByNameAndStatusFalse(String name);
    ManagementType findByName(String name);
    List<ManagementType> findAllByStatusTrue();
}
