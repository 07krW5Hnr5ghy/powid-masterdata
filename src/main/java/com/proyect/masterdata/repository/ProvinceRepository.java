package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Province;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ProvinceRepository extends JpaRepository<Province,Long> {
    Province findByName(String name);
    //Province findByDepartmentId(Long id);
    //Province findByDepartmentName(String name);
}
