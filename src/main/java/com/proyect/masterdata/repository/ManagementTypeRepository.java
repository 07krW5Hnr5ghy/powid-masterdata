package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ManagementType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ManagementTypeRepository extends JpaRepository<ManagementType,Long> {
    ManagementType findByNameAndStatusTrue(String name);
}
