package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.ClosingChannel;

@Repository
public interface ClosingChannelRepository extends JpaRepository<ClosingChannel, Long> {

    List<ClosingChannel> findByNameInAndStatusTrue(List<String> names);

    boolean existsByNameAndStatusTrue(String name);
}
