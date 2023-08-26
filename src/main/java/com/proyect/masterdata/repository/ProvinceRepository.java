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
    Province findByNameAndStatusTrue(String name);
}
