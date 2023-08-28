package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Color;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface ColorRepositoryCustom {
    Page<Color> searchForColor(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
