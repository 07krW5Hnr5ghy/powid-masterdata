package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancellationReason;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface CancellationReasonRepository extends JpaRepository<CancellationReason, UUID> {
    CancellationReason findByName(String name);
    CancellationReason findByNameAndStatusTrue(String name);
    CancellationReason findByNameAndStatusFalse(String name);
    List<CancellationReason> findAllByStatusTrue();
    List<CancellationReason> findAllByStatusFalse();
}
