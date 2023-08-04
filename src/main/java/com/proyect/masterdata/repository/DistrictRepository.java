package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.District;
import org.springframework.data.jpa.repository.JpaRepository;

public interface DistrictRepository extends JpaRepository<District,Long> {
    District findByName(String name);
    District findByProvinceId(Long id);
    District findByProvinceName(String name);
}
