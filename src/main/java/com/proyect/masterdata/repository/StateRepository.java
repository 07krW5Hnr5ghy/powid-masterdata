package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.State;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface StateRepository extends JpaRepository<State,Long> {
    List<State> findAllByStatusTrue();
    List<State> findAllByStatusFalse();
    State findByIdAndStatusTrue(Long id);
    State findByNameAndStatusTrue(String name);
    List<State> findByNameIn(List<String> names);
}
