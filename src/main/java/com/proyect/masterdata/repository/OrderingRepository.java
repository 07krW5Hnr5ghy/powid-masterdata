package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Ordering;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderingRepository extends JpaRepository<Ordering,Long> {
    List<Ordering> findAllByClientId(Long clientId);
    Ordering findByClientIdAndId(Long clientId,Long id);
}
