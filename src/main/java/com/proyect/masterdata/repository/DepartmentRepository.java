package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Department;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface DepartmentRepository extends JpaRepository<Department, Long> {
    List<Department> findAllByStatusTrue();
    Department findByIdAndStatusTrue(Long id);
    Department findByNameAndStatusTrue(String name);
    Department findByNameAndStatusFalse(String name);
    List<Department> findByNameIn(List<String> name);
}
