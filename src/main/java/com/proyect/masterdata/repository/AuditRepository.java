package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Audit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface AuditRepository extends JpaRepository<Audit,Long> {
}
