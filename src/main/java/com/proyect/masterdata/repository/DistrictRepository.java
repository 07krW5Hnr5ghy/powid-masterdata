package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.DistrictPK;
import org.springframework.data.jpa.repository.JpaRepository;

public interface DistrictRepository extends JpaRepository<District, DistrictPK> {
    District findByName(String name);
    //District findByProvinceId(Long id);
    //District findByProvinceName(String name);
}
