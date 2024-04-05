package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Store;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface StoreRepository extends JpaRepository<Store, Long> {
    List<Store> findAllByStatusTrue();
    Store findByNameAndStatusTrue(String name);
    List<Store> findByNameIn(List<String> name);
    boolean existsByName(String name);
    Store findByClientId(Long clientId);
    List<Store> findAllByClientId(Long clientId);
}
