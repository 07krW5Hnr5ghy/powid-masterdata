package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Category;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface CategoryRepository extends JpaRepository<Category,Long> {
    List<Category> findAllByStatusTrue();
    List<Category> findAllByStatusFalse();
    Category findByIdAndStatusTrue(Long id);
    Category findByNameAndStatusTrue(String name);
    Category findByDescriptionAndStatusTrue(String description);
    List<Category> findByNameIn(List<String> names);
    List<Category> findByDescriptionIn(List<String> descriptions);
}
