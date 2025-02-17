package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SubCategoryProduct;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface SubCategoryProductRepository extends JpaRepository<SubCategoryProduct, UUID> {
    SubCategoryProduct findByNameAndStatusTrue(String name);
}
