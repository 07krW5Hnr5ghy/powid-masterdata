package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.UserRole;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface UserRoleRepository extends JpaRepository<UserRole,Long> {
    List<UserRole> findAllByStatusTrue();
    List<UserRole> findAllByStatusFalse();
    UserRole findByIdAndStatusTrue(Long id);
    UserRole findByNameAndStatusTrue(String name);
    List<UserRole> findByUser(String user);
    void deleteByIdAndUser(Long id, String User);
}
