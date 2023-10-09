package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ModuleType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ModuleTypeRepository extends JpaRepository<ModuleType,Long> {
    List<ModuleType> findByModuleIn(List<String> modules);
}
