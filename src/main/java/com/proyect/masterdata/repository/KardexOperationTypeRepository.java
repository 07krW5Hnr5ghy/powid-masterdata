package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.KardexOperationType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;
@Repository
public interface KardexOperationTypeRepository extends JpaRepository<KardexOperationType, UUID> {
    List<KardexOperationType> findAllByStatusTrueAndClientId(UUID clientId);
    List<KardexOperationType> findAllByStatusFalseAndClientId(UUID clientId);
    KardexOperationType findByIdAndClientIdAndStatusTrue(UUID id,UUID clientId);
    KardexOperationType findByNameAndClientIdAndStatusTrue(String name,UUID clientId);
    KardexOperationType findByNameAndClientIdAndStatusFalse(String name,UUID clientId);
    KardexOperationType findByNameAndClientId(String name,UUID clientId);
    List<KardexOperationType> findByClientIdAndNameIn(UUID clientId,List<String> names);
    List<KardexOperationType> findAllByClientId(UUID clientId);
}
