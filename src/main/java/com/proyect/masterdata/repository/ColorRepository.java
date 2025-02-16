package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Color;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface ColorRepository extends JpaRepository<Color, UUID> {
    List<Color> findAllByStatusTrue();
    List<Color> findAllByStatusFalse();
    Color findByIdAndStatusTrue(UUID id);
    Color findByNameAndStatusTrue(String name);
    Color findByNameAndStatusFalse(String name);
    List<Color> findByNameIn(List<String> names);
}
