package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.CategoryProduct;

@Repository
public interface CategoryProductRepository extends JpaRepository<CategoryProduct, Long> {
    CategoryProduct findByName(String name);

    List<CategoryProduct> findByNameIn(List<String> names);

    CategoryProduct findByNameAndStatusTrue(String name);
}
