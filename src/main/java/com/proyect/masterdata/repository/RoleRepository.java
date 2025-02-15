package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Role;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Set;
import java.util.UUID;

@Repository
public interface RoleRepository extends JpaRepository<Role, UUID> {
    List<Role> findAllByStatusTrue();
    List<Role> findAllByStatusFalse();
    Role findByIdAndStatusTrue(UUID id);
    Role findByName(String name);
    Role findByNameAndStatusTrue(String name);
    Role findByNameAndStatusFalse(String name);
    Set<Role> findByNameIn(List<String> names);
    List<Role> findRoleByNameIn(List<String> names);
}
