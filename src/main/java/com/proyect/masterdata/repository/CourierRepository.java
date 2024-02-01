package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Courier;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CourierRepository extends JpaRepository<Courier,Long> {
    Courier findByName(String name);
    Courier findByNameAndStatusTrue(String name);
}
