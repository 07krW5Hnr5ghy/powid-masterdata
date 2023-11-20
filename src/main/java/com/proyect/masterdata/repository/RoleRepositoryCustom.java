package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Role;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface RoleRepositoryCustom {
    Page<Role> searchForRole(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
