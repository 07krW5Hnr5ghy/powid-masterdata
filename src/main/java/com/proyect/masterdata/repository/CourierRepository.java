package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Courier;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CourierRepository extends JpaRepository<Courier,Long> {
    Courier findByName(String name);
    Courier findByNameAndStatusTrue(String name);
    Courier findByNameAndStatusFalse(String name);
    List<Courier> findAllByClientIdAndStatusTrue(Long clientId);
    List<Courier> findAllByClientIdAndStatusFalse(Long clientId);
}
