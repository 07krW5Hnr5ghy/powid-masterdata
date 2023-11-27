package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.EntryChannel;

@Repository
public interface EntryChannelRepository extends JpaRepository<EntryChannel, Long> {
    EntryChannel findByNameAndStatusTrue(String name);
}
