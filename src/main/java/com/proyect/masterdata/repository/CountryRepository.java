package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Country;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CountryRepository extends JpaRepository<Country,Long> {
    Country findByNameAndStatusTrue(String name);
    List<Country> findByNameIn(List<String> names);
    List<Country> findAllByStatusTrue();
}
