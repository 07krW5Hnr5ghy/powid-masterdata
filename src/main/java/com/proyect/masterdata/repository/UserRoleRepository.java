package com.proyect.masterdata.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.UserRole;

@Repository
public interface UserRoleRepository extends JpaRepository<UserRole, Long> {
    List<UserRole> findByUserIdAndStatusTrue(Long userId);
    UserRole findByUserIdAndRoleIdAndStatusTrue(Long userId,Long roleId);
    UserRole findByUserIdAndRoleIdAndStatusFalse(Long userId,Long roleId);
}
