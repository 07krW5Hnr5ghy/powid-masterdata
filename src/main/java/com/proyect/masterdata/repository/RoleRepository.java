package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Role;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Set;

public interface RoleRepository extends JpaRepository<Role, Long> {
    List<Role> findAllByStatusTrue();

    List<Role> findAllByStatusFalse();

    Role findByIdAndStatusTrue(Long id);

    Role findByNameAndStatusTrue(String name);

    Set<Role> findByNameIn(List<String> names);

    List<Role> findRoleByNameIn(List<String> names);
}
