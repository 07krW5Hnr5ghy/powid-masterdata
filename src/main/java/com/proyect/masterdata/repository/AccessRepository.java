package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Access;

@Repository
public interface AccessRepository extends JpaRepository<Access, Long> {

    Access findByName(String name);

    Access findByNameAndStatusTrue(String name);

    List<Access> findByNameInAndStatusTrue(List<String> names);

}
