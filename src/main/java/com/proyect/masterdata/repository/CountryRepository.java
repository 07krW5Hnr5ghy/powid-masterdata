package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Country;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface CountryRepository extends JpaRepository<Country, UUID> {
    Country findByNameAndStatusTrue(String name);
    List<Country> findByNameIn(List<String> names);
    List<Country> findAllByStatusTrue();
}
