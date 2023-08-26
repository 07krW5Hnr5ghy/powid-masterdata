package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.District;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface DistrictRepository extends JpaRepository<District, Long> {
    boolean existsByName(String name);
    List<District> findByNameIn(List<String> name);
    List<District> findAllByStatusTrue();
    District findByNameAndStatusTrue(String name);
}