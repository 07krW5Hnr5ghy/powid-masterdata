package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Size;
import org.springframework.data.jpa.repository.JpaRepository;

public interface SizeRepository extends JpaRepository<Size,Long> {
    Size findByName(String name);
}
