package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Role;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface RoleRepository extends JpaRepository<Role, Long> {
    List<Role> findAllByStatusTrue();

    List<Role> findAllByStatusFalse();

    Role findByIdAndStatusTrue(Long id);

    Role findByNameAndStatusTrue(String name);

    List<Role> findByNameIn(List<String> names);
}
