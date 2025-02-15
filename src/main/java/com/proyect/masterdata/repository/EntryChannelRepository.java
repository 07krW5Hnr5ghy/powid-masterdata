package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.EntryChannel;

@Repository
public interface EntryChannelRepository extends JpaRepository<EntryChannel, UUID> {
    EntryChannel findByNameAndStatusTrue(String name);
    EntryChannel findByNameAndStatusFalse(String name);
    boolean existsByNameAndStatusTrue(String name);
    List<EntryChannel> findAllByStatusTrue();
}
