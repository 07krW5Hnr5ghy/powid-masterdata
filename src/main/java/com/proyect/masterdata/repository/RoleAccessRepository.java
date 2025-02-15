package com.proyect.masterdata.repository;

import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.RoleAccess;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;

@Repository
public interface RoleAccessRepository extends JpaRepository<RoleAccess, UUID> {
    List<RoleAccess> findByRoleId(UUID roleId);
    RoleAccess findByRoleIdAndAccessId(UUID roleId,UUID AccessId);
    RoleAccess findByRoleIdAndAccessIdAndStatusTrue(UUID roleId,UUID AccessId);
}
