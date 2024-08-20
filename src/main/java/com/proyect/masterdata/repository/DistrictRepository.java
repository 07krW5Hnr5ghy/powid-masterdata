package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.District;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface DistrictRepository extends JpaRepository<District, Long> {
    boolean existsByName(String name);
    List<District> findByNameIn(List<String> name);
    List<District> findAllByStatusTrue();
    District findByNameAndStatusTrue(String name);
    District findByNameAndStatusFalse(String name);
    List<District> findAllByProvinceIdAndStatusTrue(Long provinceId);
}