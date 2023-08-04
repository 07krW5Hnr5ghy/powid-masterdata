package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.State;
import org.springframework.data.jpa.repository.JpaRepository;

public interface StateRepository extends JpaRepository<State,Long> {
    State findByName(String name);
}
