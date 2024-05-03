package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Access;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface AccessRepositoryCustom {
    Page<Access> searchForAccess(
            String name,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
