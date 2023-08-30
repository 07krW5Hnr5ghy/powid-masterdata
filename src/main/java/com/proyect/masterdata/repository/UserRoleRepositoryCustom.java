package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.UserRole;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface UserRoleRepositoryCustom {
    Page<UserRole> searchForUserRole(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
