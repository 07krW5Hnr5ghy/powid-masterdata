package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Size;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface SizeRepositoryCustom {
    Page<Size> searchForSize(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
