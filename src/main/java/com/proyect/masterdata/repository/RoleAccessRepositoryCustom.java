package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.RoleAccess;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface RoleAccessRepositoryCustom {
    Page<RoleAccess> searchForRoleAccess(
            UUID roleId,
            UUID accessId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
