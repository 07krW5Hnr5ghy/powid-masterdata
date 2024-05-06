package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ManagementType;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface ManagementTypeRepositoryCustom {
    Page<ManagementType> searchForManagementType(
            String name,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
