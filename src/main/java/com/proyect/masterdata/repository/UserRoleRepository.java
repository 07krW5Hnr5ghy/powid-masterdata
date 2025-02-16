package com.proyect.masterdata.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.UserRole;

@Repository
public interface UserRoleRepository extends JpaRepository<UserRole, UUID> {
    List<UserRole> findByUserIdAndStatusTrue(UUID userId);
    UserRole findByUserIdAndRoleIdAndStatusTrue(UUID userId,UUID roleId);
    UserRole findByUserIdAndRoleIdAndStatusFalse(UUID userId,UUID roleId);
}
