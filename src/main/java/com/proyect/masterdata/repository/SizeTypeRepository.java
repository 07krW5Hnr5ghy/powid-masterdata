package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SizeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface SizeTypeRepository extends JpaRepository<SizeType, UUID> {
    List<SizeType> findAllByStatusTrue();
    List<SizeType> findAllByStatusFalse();
    SizeType findByIdAndStatusTrue(UUID id);
    SizeType findByNameAndStatusTrue(String name);
    SizeType findByNameAndStatusFalse(String name);
    SizeType findByName(String name);
    boolean existsByName(String name);
    List<SizeType> findByNameIn(List<String> names);
}
