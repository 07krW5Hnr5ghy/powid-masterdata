package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.LogEvent;
import org.springframework.data.jpa.repository.JpaRepository;

public interface LogEventRepository extends JpaRepository<LogEvent,Long> {
    Department findByName(String name);
}
