package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Restock;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface RestockRepository extends JpaRepository<Restock,Long> {
}
