package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Category;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {
    List<Category> findAllByStatusTrue();
    List<Category> findAllByStatusFalse();
    Category findByIdAndStatusTrue(Long id);
    Category findByNameAndStatusTrue(String name);
    Category findByName(String name);
    Boolean existsByNameAndStatusTrue(String name);
    Category findByDescriptionAndStatusTrue(String description);
    Category findByDescription(String name);
    List<Category> findByNameIn(List<String> names);
    List<Category> findByDescriptionIn(List<String> descriptions);
}
