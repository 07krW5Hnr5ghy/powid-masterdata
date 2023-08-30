package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SizeType;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface SizeTypeRepositoryCustom {
    Page<SizeType> searchForSizeType(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
