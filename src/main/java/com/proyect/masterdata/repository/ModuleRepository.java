package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Module;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface ModuleRepository extends JpaRepository<Module, UUID> {
    List<Module> findAllByStatusTrue();

    Module findByIdAndStatusTrue(UUID id);

    Module findByNameAndStatusTrue(String name);

    List<Module> findByNameIn(List<String> names);

    boolean existsByName(String name);
}
