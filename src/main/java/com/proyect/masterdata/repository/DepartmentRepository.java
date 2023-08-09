package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Department;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface DepartmentRepository extends JpaRepository<Department, Long> {
    List<Department> findAllByStatusTrue();
    List<Department> findAllByStatusFalse();
    Department findByIdAndStatusTrue(Long id);
    Department findByNameAndStatusTrue(String name);
    List<Department> findByUser(String user);

    void deleteByIdAndUser(Long id, String User);
}
