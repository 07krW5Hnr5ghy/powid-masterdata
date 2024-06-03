package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.AuditEvent;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface AuditEventRepository extends JpaRepository<AuditEvent,Long> {
    AuditEvent findByName(String name);
    AuditEvent findByNameAndStatusTrue(String name);
    List<AuditEvent> findAllByStatusTrue();
}
