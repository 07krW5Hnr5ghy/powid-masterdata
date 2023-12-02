package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {

    List<User> findByUsernameIn(List<String> name);

    User findByUsernameAndStatusTrue(String user);

    boolean existsByUsername(String user);

    boolean existsByUsernameAndStatusTrue(String user);

    boolean existsByDni(String dni);

    boolean existsByEmail(String email);

    boolean existsByMobile(String mobile);

    List<User> findAll();
}
