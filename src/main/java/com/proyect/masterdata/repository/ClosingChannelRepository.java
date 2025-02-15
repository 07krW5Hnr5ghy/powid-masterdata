package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.ClosingChannel;

@Repository
public interface ClosingChannelRepository extends JpaRepository<ClosingChannel, UUID> {
    List<ClosingChannel> findByNameInAndStatusTrue(List<String> names);
    boolean existsByNameAndStatusTrue(String name);
    List<ClosingChannel> findAllByStatusTrue();
    ClosingChannel findByNameAndStatusTrue(String name);
    ClosingChannel findByNameAndStatusFalse(String name);
}
