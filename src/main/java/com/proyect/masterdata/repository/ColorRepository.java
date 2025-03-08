package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Color;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface ColorRepository extends JpaRepository<Color, UUID> {
    List<Color> findAllByStatusTrueAndClientId(UUID clientId);
    List<Color> findAllByStatusFalseAndClientId(UUID clientId);
    Color findByIdAndClientIdAndStatusTrue(UUID id,UUID clientId);
    Color findByNameAndClientIdAndStatusTrue(String name,UUID clientId);
    Color findByNameAndClientIdAndStatusFalse(String name,UUID clientId);
    Color findByNameOrSkuAndClientId(String name,String sku,UUID clientId);
    List<Color> findByClientIdAndNameIn(UUID clientId,List<String> names);
    List<Color> findAllByClientId(UUID clientId);
}
