package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Province;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
@Repository
public interface ProvinceRepository extends JpaRepository<Province, Long> {

    boolean existsByName(String name);
    List<Province> findByNameIn(List<String> name);
    List<Province> findAllByStatusTrue();
    List<Province> findAllByStatusFalse();
    List<Province> findByUser(String user);
    List<Province> findAllByStatusTrueAndDepartmentId(Long id);
    List<Province> findAllByStatusTrueAndDepartmentName(String name);
    Province findByIdAndStatusTrue(Long id);
    Province findByNameAndStatusTrue(String name);

    void deleteByIdAndUser(Long id, String User);
}
