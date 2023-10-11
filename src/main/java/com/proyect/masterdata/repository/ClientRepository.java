package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Client;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ClientRepository extends JpaRepository<Client, Long> {
    boolean existsByRuc(String business);
    List<Client> findByRucIn(List<String> rucList);
}
