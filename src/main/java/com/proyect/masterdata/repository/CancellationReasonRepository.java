package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancellationReason;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CancellationReasonRepository extends JpaRepository<CancellationReason,Long> {
    CancellationReason findByName(String name);
    CancellationReason findByNameAndStatusTrue(String name);
    List<CancellationReason> findAllByStatusTrue();
}
