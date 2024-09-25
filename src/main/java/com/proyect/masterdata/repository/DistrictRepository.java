package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.District;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface DistrictRepository extends JpaRepository<District, Long> {
    District findByNameAndProvinceId(String name,Long provinceId);
    List<District> findByNameIn(List<String> name);
    List<District> findAllByStatusTrue();
    District findByNameAndProvinceIdAndStatusTrue(String name,Long provinceId);
    District findByNameAndProvinceIdAndStatusFalse(String name,Long provinceId);
    List<District> findAllByProvinceIdAndStatusTrue(Long provinceId);
    List<District> findAllByProvinceId(Long provinceId);
}