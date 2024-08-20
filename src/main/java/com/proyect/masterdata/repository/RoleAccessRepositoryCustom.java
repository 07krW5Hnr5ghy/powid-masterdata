package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.RoleAccess;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface RoleAccessRepositoryCustom {
    Page<RoleAccess> searchForRoleAccess(
            Long roleId,
            Long accessId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
