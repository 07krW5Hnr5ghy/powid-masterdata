package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Connection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ConnectionRepository extends JpaRepository<Connection,Long> {
    boolean existsByUrl(String url);
    Connection findByUrl(String url);
    List<Connection> findByUrlIn(List<String> urlList);

}
