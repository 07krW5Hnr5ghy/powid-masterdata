package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ClientChannel;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ClientChannelRepository extends JpaRepository<ClientChannel,Long> {
    List<ClientChannel> findAllByStatusTrue();
    ClientChannel findByIdAndStatusTrue(Long id);
    ClientChannel findByIdClient(Long id);
    List<ClientChannel> findByNameIn(List<String> name);
    boolean existsByName(String name);
}
