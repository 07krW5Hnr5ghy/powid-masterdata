package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.DistrictPK;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface DistrictRepository extends JpaRepository<District, DistrictPK> {
    List<District> findAllByStatusTrue();
    List<District> findAllByStatusFalse();
    List<District> findAllByStatusTrueAndProvinceId(Long id);
    List<District> findAllByStatusTrueAndProvinceName(String name);
    List<District> findByLoginUser(String user);
    District findByIdAndStatusTrue(Long id);
    District findByNameAndStatusTrue(String name);
}
