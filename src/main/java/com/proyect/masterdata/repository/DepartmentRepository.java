package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.DepartmentPK;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface DepartmentRepository extends JpaRepository<Department, DepartmentPK> {
    List<Department> findAllByStatusTrue();
    List<Department> findAllByStatusFalse();
    Department findByIdAndStatusTrue(Long id);
    Department findByNameAndStatusTrue(String name);
    Department findByLoginUser(String user);
}
