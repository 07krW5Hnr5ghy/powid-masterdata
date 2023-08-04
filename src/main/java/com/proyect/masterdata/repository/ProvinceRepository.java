package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.domain.ProvincePK;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ProvinceRepository extends JpaRepository<Province, ProvincePK> {
    List<Province> findAllByStatusTrue();
    List<Province> findAllByStatusFalse();
    List<Province> findAllByStatusTrueAndDepartmentId(Long id);
    List<Province> findAllByStatusTrueAndDepartmentName(String name);
    List<Province> findByLoginUser(String user);
    Province findByIdAndStatusTrue(Long id);
    Province findByNameAndStatusTrue(String name);
}
