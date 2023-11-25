package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Color;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ColorRepository extends JpaRepository<Color, Long> {
    List<Color> findAllByStatusTrue();

    List<Color> findAllByStatusFalse();

    Color findByIdAndStatusTrue(Long id);

    Color findByNameAndStatusTrue(String name);

    List<Color> findByNameIn(List<String> names);
}
