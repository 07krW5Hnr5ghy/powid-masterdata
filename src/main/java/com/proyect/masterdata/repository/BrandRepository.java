package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Brand;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface BrandRepository extends JpaRepository<Brand, Long> {
    boolean existsByName(String name);
    Brand findByName(String name);
    Brand findByNameAndStatusTrue(String name);
    Brand findByNameAndStatusFalse(String name);
    List<Brand> findByNameIn(List<String> namesList);
    List<Brand> findAllByClientIdAndStatusTrue(Long clientId);
    List<Brand> findAllByClientIdAndStatusFalse(Long clientId);
    List<Brand> findAllByClientId(Long clientId);
}
