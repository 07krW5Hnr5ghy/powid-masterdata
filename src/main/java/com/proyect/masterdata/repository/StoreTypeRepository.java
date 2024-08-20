package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.StoreType;

public interface StoreTypeRepository extends JpaRepository<StoreType, Long> {
    StoreType findByNameAndStatusTrue(String name);
    StoreType findByNameAndStatusFalse(String name);
    List<StoreType> findByNameInAndStatusTrue(List<String> nameList);
    boolean existsByName(String name);
    List<StoreType> findAllByStatusTrue();

}
