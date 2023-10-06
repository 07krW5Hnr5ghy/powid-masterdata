package com.proyect.masterdata.repository;


import com.proyect.masterdata.domain.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface UserRepository extends JpaRepository<User, String> {
    List<User> findByNameIn(List<String> name);
    User findByUser(String user);
    boolean existsByUser(String user);
}
