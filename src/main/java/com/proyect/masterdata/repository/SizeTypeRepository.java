package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SizeType;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface SizeTypeRepository extends JpaRepository<SizeType,Long> {
    List<SizeType> findAllByStatusTrue();
    List<SizeType> findAllByStatusFalse();
    SizeType findByIdAndStatusTrue(Long id);
    SizeType findByNameAndStatusTrue(String name);
    List<SizeType> findByNameIn(List<String> names);
}
