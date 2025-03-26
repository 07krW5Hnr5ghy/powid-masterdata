package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Courier;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface CourierRepository extends JpaRepository<Courier, UUID> {
    Courier findByNameOrDniAndClientId(String name,String dni,UUID clientId);
    Courier findByNameAndClientIdAndStatusTrue(String name,UUID clientId);
    Courier findByDniAndClientIdAndStatusTrue(String dni,UUID clientId);
    Courier findByDniAndClientIdAndStatusFalse(String dni,UUID clientId);
    Courier findByDniAndClientId(String dni,UUID clientId);
    Courier findByNameAndStatusTrue(String name);
    Courier findByNameAndClientIdAndStatusFalse(String name,UUID clientId);
    List<Courier> findAllByClientIdAndStatusTrue(UUID clientId);
    List<Courier> findAllByClientIdAndStatusFalse(UUID clientId);
    List<Courier> findAllByClientId(UUID clientId);
    List<Courier> findByClientIdAndNameIn(UUID clientId,List<String> names);
}
