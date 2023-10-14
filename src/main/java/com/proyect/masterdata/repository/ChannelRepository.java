package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Channel;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ChannelRepository extends JpaRepository<Channel,Long> {
    boolean existsByName(String name);
    List<Channel> findByNameIn(List<String> names);
    Channel findByName(String name);
}
