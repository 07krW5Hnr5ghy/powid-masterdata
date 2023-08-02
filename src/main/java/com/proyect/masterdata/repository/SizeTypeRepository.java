package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.SizeType;
import org.springframework.data.jpa.repository.JpaRepository;

public interface SizeTypeRepository extends JpaRepository<SizeType,Long> {
    Department findByName(String name);
}
