package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.District;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;
import java.util.UUID;

@Repository
public interface DistrictRepository extends JpaRepository<District, UUID> {
    District findByNameAndProvinceId(String name,UUID provinceId);
    List<District> findByNameIn(List<String> name);
    List<District> findAllByStatusTrue();
    District findByNameAndStatusTrue(String name);
    District findByNameAndProvinceIdAndStatusTrue(String name,UUID provinceId);
    District findByNameAndProvinceIdAndStatusFalse(String name,UUID provinceId);
    List<District> findAllByProvinceIdAndStatusTrue(UUID provinceId);
    List<District> findAllByProvinceId(UUID provinceId);

    @Query(value = "SELECT " +
            "de.name AS department, " +
            "p.name AS province, " +
            "di.name AS district " +
            "FROM master.department de " +
            "JOIN master.province p ON p.department_id = de.department_id " +
            "JOIN master.district di ON di.province_id = p.province_id " , nativeQuery = true)
    List<Object[]> findDepartmentsProvincesDistricts();
}