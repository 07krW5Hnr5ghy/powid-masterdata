package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.District;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface DistrictRepository extends JpaRepository<District, Long> {
    List<District> findAllByStatusTrue();
    List<District> findAllByStatusFalse();
    List<District> findAllByStatusTrueAndProvinceId(Long id);
    List<District> findAllByStatusTrueAndProvinceName(String name);
    List<District> findByUser(String user);
    District findByIdAndStatusTrue(Long id);
    District findByNameAndStatusTrue(String name);

    void deleteByIdAndUser(Long id, String User);
}
