package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.projections.ProvinceDTOP;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import java.util.List;
import java.util.UUID;

@Repository
public interface ProvinceRepository extends JpaRepository<Province, UUID> {
    boolean existsByName(String name);
    List<Province> findByNameIn(List<String> name);
    List<Province> findAllByStatusTrue();
    Province findByNameAndStatusTrue(String name);
    Province findByNameAndStatusFalse(String name);
    List<Province> findAllByDepartmentIdAndStatusTrue(UUID departmentId);
    List<Province> findAllByDepartmentId(UUID departmentId);
    ProvinceDTOP findByName(String name);
}
