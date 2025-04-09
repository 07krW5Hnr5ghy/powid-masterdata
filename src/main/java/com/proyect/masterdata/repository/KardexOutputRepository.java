package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.KardexOutput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface KardexOutputRepository extends JpaRepository<KardexOutput, UUID> {

}
