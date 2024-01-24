package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancellationReason;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CancellationReasonRepository extends JpaRepository<CancellationReason,Long> {
}
