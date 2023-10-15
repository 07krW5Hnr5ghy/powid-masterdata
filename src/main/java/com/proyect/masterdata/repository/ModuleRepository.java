package com.proyect.masterdata.repository;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.domain.UserType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ModuleRepository extends JpaRepository<Module,Long> {
    List<Module> findAllByStatusTrue();
    Module findByIdAndStatusTrue(Long id);
    Module findByNameAndStatusTrue(String name);
    List<Module> findByNameIn(List<String> names);
    boolean existsByName(String name);
}
