package com.proyect.masterdata.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.proyect.masterdata.domain.EntryChannel;

public interface EntryChannelRepository extends JpaRepository<EntryChannel, Long> {
    EntryChannel findByNameAndStatusTrue(String name);
}
