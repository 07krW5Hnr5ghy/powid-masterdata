package com.proyect.masterdata.repository;
import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.Module;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ModuleRepository extends JpaRepository<Module,Long> {
    Department findByName(String name);
}
