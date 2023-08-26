package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Size;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface SizeRepository extends JpaRepository<Size,Long> {
    boolean existsByName(String name);
    List<Size> findAllByStatusTrue();
    List<Size> findAllByStatusFalse();
    List<Size> findAllByStatusTrueAndSizeTypeId(Long id);
    List<Size> findAllByStatusTrueAndSizeTypeName(String name);
    Size findByIdAndStatusTrue(Long id);
    Size findByNameAndStatusTrue(String name);
    List<Size> findByNameIn(List<String> names);
}
