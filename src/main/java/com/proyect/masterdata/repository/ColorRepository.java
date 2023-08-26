package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Color;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ColorRepository extends JpaRepository<Color,Long> {
    List<Color> findAllByStatusTrue();
    List<Color> findAllByStatusFalse();
    Color findByIdAndStatusTrue(Long id);
    Color findByNameAndStatusTrue(String name);
    List<Color> findByNameIn(List<String> names);
    List<Color> findByUser(String user);
    void deleteByIdAndUser(Long id, String User);
}
