package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Store;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface StoreRepository extends JpaRepository<Store, UUID> {
    List<Store> findAllByStatusTrue();
    Store findByNameAndStatusTrue(String name);
    Store findByNameAndStatusFalse(String name);
    List<Store> findByNameIn(List<String> name);
    boolean existsByName(String name);
    Store findByClientId(UUID clientId);
    List<Store> findAllByClientId(UUID clientId);
}
