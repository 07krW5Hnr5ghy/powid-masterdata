package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Size;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface SizeRepository extends JpaRepository<Size, UUID> {
    Size findByNameAndClientId(String name,UUID clientId);
    List<Size> findAllByStatusTrueAndClientId(UUID clientId);
    List<Size> findAllByStatusFalseAndClientId(UUID clientId);
    List<Size> findAllByStatusTrueAndSizeTypeId(UUID id);
    List<Size> findAllByStatusTrueAndSizeTypeNameAndClientId(String name,UUID clientId);
    Size findByIdAndStatusTrue(UUID id);
    Size findByNameAndStatusTrueAndClientId(String name,UUID clientId);
    Size findByNameAndStatusFalseAndClientId(String name,UUID clientId);
    List<Size> findByNameInAndClientId(List<String> names,UUID clientId);
    List<Size> findAllByClientId(UUID clientId);
}
