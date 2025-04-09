package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.KardexInput;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface KardexInputRepository extends JpaRepository<KardexInput, UUID> {
    Long countByClientIdAndProductId(UUID clientId,UUID productId);

}
