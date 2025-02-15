package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Size;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface SizeRepository extends JpaRepository<Size, UUID> {
    boolean existsByName(String name);
    List<Size> findAllByStatusTrue();
    List<Size> findAllByStatusFalse();
    List<Size> findAllByStatusTrueAndSizeTypeId(UUID id);
    List<Size> findAllByStatusTrueAndSizeTypeName(String name);
    Size findByIdAndStatusTrue(UUID id);
    Size findByNameAndStatusTrue(String name);
    Size findByNameAndStatusFalse(String name);
    List<Size> findByNameIn(List<String> names);
}
