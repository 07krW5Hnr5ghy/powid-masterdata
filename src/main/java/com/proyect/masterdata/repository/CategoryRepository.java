package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.domain.Department;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CategoryRepository extends JpaRepository<Category,Long> {
    Department findByName(String name);
}
