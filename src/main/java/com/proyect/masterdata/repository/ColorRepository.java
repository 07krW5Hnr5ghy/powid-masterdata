package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.domain.Department;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ColorRepository extends JpaRepository<Color,Long> {
    Department findByName(String name);
}
