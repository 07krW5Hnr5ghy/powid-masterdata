package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Client;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ClientRepository extends JpaRepository<Client, Long> {
    boolean existsByRuc(String ruc);
    boolean existsByBusiness(String business);
    boolean existsByDni(String dni);
    boolean existsByEmail(String email);
    boolean existsByMobile(String mobile);
    Client findByRucAndStatusTrue(String ruc);
    List<Client> findByRucIn(List<String> rucList);
    Client findByRuc(String ruc);
    Client findByRucAndStatusFalse(String ruc);
}
