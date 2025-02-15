package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Department;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface DepartmentRepository extends JpaRepository<Department, UUID> {
    List<Department> findAllByStatusTrue();
    Department findByIdAndStatusTrue(UUID id);
    Department findByNameAndStatusTrue(String name);
    Department findByNameAndStatusFalse(String name);
    List<Department> findByNameIn(List<String> name);
}
