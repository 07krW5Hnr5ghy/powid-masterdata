package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Access;

@Repository
public interface AccessRepository extends JpaRepository<Access, Long> {

    Boolean existsByName(String name);

}
