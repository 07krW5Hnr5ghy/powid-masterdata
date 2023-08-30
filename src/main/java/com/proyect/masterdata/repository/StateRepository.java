package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.State;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
@Repository
public interface StateRepository extends JpaRepository<State,Long> {
    List<State> findAllByStatusTrue();
    List<State> findAllByStatusFalse();
    State findByIdAndStatusTrue(Long id);
    State findByNameAndStatusTrue(String name);
    List<State> findByNameIn(List<String> names);
}
