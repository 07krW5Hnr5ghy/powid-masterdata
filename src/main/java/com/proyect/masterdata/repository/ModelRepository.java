package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Model;

@Repository
public interface ModelRepository extends JpaRepository<Model, Long> {

    boolean existsByName(String name);

}
