package com.proyect.masterdata.repository;

import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.RoleAccess;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

@Repository
public interface RoleAccessRepository extends JpaRepository<RoleAccess, Long> {
    List<RoleAccess> findByRoleId(Long roleId);
    RoleAccess findByRoleIdAndAccessId(Long roleId,Long AccessId);
    RoleAccess findByRoleIdAndAccessIdAndStatusTrue(Long roleId,Long AccessId);
}
